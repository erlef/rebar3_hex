%% @doc `rebar3 hex outdated' - Check for outdated dependencies
%%
%% Displays a table of Hex package dependencies declared in `rebar.config' showing the
%% currently configured version alongside the latest version available on the
%% repository, and whether an update is possible given the current version
%% requirement.
%%
%% ```
%% $ rebar3 hex outdated
%% Dependency       Only   Current   Latest   Status
%%
%% hex_core                0.17.0    0.18.0   Update not possible
%% verl                    1.1.1     1.1.1    Up-to-date
%% erlware_commons  test   1.7.0     1.8.1    Update not possible
%% elli             test   3.3.0     3.3.0    Up-to-date
%% jsone            test   1.8.1     1.9.0    Update not possible
%% meck             test   0.9.2     1.2.0    Update not possible
%% '''
%%
%% <h2> Command line options </h2>
%%
%% <ul>
%%  <li>`--repo' - Specify the repository to work with. This option is required when you
%%   have multiple repositories configured, including organizations. The argument must be a fully qualified repository
%%   name (e.g, `hexpm', `hexpm:my_org', `my_own_hexpm').
%%   Defaults to `hexpm'.
%%   </li>
%% </ul>
-module(rebar3_hex_outdated).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, outdated).
-define(DEPS, []).

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex outdated"},
                                {short_desc, "Display outdated Hex dependencies"},
                                {desc, ""},
                                {opts, [rebar3_hex:repo_opt()]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    case rebar3_hex_config:repo(State) of
        {ok, Repo} ->
            HexConfig = rebar3_hex_config:get_hex_config(?MODULE, Repo, read),
            Deps = gather_deps(State),
            Locks = rebar_state:get(State, {locks, default}, []),
            Rows = lists:filtermap(fun(Dep) -> dep_row(HexConfig, Locks, Dep) end, Deps),
            print(Rows),
            {ok, State};
        {error, Reason} ->
            ?RAISE(Reason)
    end.

%% @private
-spec format_error(any()) -> iolist().
format_error({status, Status}) ->
    io_lib:format("Error fetching package information: ~ts",
                  [rebar3_hex_client:pretty_print_status(Status)]);
format_error({error, Reason}) ->
    io_lib:format("Error fetching package information: ~p", [Reason]);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% Returns a list of {Name, Requirement, Only} where `Only' is `default' for
%% top level deps, or the profile name for deps only declared in a profile.
gather_deps(State) ->
    Dir = rebar_state:dir(State),
    Config = rebar_config:consult(Dir),
    DefaultDeps = proplists:get_value(deps, Config, []),
    Profiles = proplists:get_value(profiles, Config, []),
    DefaultNames = [dep_name(D) || D <- DefaultDeps],
    DefaultRows = [{dep_name(D), dep_requirement(D), default} || D <- DefaultDeps],
    ProfileRows = lists:flatmap(
                    fun({ProfileName, ProfileOpts}) ->
                            ProfileDeps = proplists:get_value(deps, ProfileOpts, []),
                            [{dep_name(D), dep_requirement(D), ProfileName}
                             || D <- ProfileDeps, not lists:member(dep_name(D), DefaultNames)]
                    end,
                    Profiles),
    DefaultRows ++ ProfileRows.

dep_name(Name) when is_atom(Name) -> Name;
dep_name({Name, _Vsn}) -> Name;
dep_name({Name, _Vsn, _Opts}) -> Name;
dep_name({Name, _Vsn, _Type, _Source}) -> Name;
dep_name(Name) -> Name.

dep_requirement(Name) when is_atom(Name) -> undefined;
dep_requirement({_Name, Vsn}) when is_list(Vsn); is_binary(Vsn) -> rebar_utils:to_binary(Vsn);
dep_requirement({_Name, Vsn, _Opts}) when is_list(Vsn); is_binary(Vsn) -> rebar_utils:to_binary(Vsn);
dep_requirement(_) -> undefined.

%% Builds a table row for a single dependency. Returns `false' for
%% dependencies that aren't simple Hex package requirements (e.g. git deps)
%% so they're skipped.
dep_row(_HexConfig, _Locks, {_Name, undefined, _Only}) ->
    false;
dep_row(HexConfig, Locks, {Name, Requirement, Only}) ->
    NameBin = rebar_utils:to_binary(Name),
    case hex_api_package:get(HexConfig, NameBin) of
        {ok, {200, _Headers, Package}} ->
            Current = current_version(Name, Requirement, Locks),
            Latest = latest_stable(maps:get(<<"releases">>, Package, [])),
            Status = status(Current, Latest, Requirement),
            {true, [rebar_utils:to_list(Name),
                    only_label(Only),
                    rebar_utils:to_list(Current),
                    rebar_utils:to_list(Latest),
                    Status]};
        {ok, {404, _Headers, _Body}} ->
            false;
        {ok, {Status, _Headers, _Body}} ->
            throw(?PRV_ERROR({status, Status}));
        {error, Reason} ->
            throw(?PRV_ERROR({error, Reason}))
    end.

only_label(default) -> "";
only_label(Profile) -> rebar_utils:to_list(Profile).

%% The locked version takes precedence as the "current" version since it's
%% what is actually used when building. Fall back to the requirement as
%% configured in rebar.config for deps that have no lock entry (e.g.
%% profile-only deps).
current_version(Name, Requirement, Locks) ->
    NameBin = rebar_utils:to_binary(Name),
    case lists:keyfind(NameBin, 1, Locks) of
        {NameBin, {pkg, _PkgName, Version, _, _}, _Level} ->
            Version;
        {NameBin, {pkg, _PkgName, Version, _}, _Level} ->
            Version;
        false ->
            Requirement
    end.

latest_stable(Releases) ->
    case gather_stable_releases(Releases) of
        [] ->
            <<"-">>;
        [Latest | _Rest] ->
            maps:get(<<"version">>, Latest)
    end.

gather_stable_releases(Releases) ->
    version_sort(
        lists:filter(
            fun(#{<<"version">> := Ver}) ->
            case verl:parse(Ver) of
                {ok, #{pre := []}} -> true;
                _ -> false
            end
        end,
        Releases)).

version_sort(Releases) ->
    lists:sort(
        fun(#{<<"version">> := A}, #{<<"version">> := B}) -> verl:gte(A, B) end,
        Releases
    ).

status(Latest, Latest, _Requirement) ->
    "Up-to-date";
status(_Current, Latest, Requirement) ->
    case verl:is_match(Latest, Requirement) of
        true -> "Update possible";
        _    -> "Update not possible"
    end.

print(Rows) ->
    Header = ["Dependency", "Only", "Current", "Latest", "Status"],
    Colorizers = #{4 => fun latest_color/1, 5 => fun status_color/1},
    ok = rebar3_hex_results:print_table([Header | Rows], Colorizers).

latest_color([_Name, _Only, _Current, _Latest, "Up-to-date"]) -> green;
latest_color(_Row) -> red.

status_color([_Name, _Only, _Current, _Latest, "Up-to-date"]) -> green;
status_color([_Name, _Only, _Current, _Latest, "Update possible"]) -> yellow;
status_color(_Row) -> red.
