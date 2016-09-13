-module(rebar3_hex_cut).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, cut).
-define(DEPS, [{default, lock}]).

-define(ENDPOINT, "packages").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex cut"},
                                {short_desc, "Increment version number and publish package"},
                                {desc, ""},
                                {opts, [{increment, $i, "increment", string, "Type of semver increment: major, minor or patch"}]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(increment, Args, undefined) of
        undefined ->
            Apps = rebar3_hex_utils:select_apps(rebar_state:project_apps(State)),
            lists:foldl(fun(App, {ok, StateAcc}) ->
                                do_(App, StateAcc)
                        end, {ok, State}, Apps);
         Type ->
            case string_to_bump(Type) of
                error ->
                    {error, {?MODULE, {bad_increment, Type}}};
                Bump ->
                    Apps = rebar3_hex_utils:select_apps(rebar_state:project_apps(State)),
                    lists:foldl(fun(App, {ok, StateAcc}) ->
                                do_(Bump, App, StateAcc)
                        end, {ok, State}, Apps)
            end
    end.

-spec format_error(any()) -> iolist().
format_error({bad_increment, Type}) ->
    io_lib:format("Increment must be major, minor or patch. ~s is not valid.", [Type]);
format_error(Error) ->
    io_lib:format("~p", [Error]).

%% ===================================================================
%% Public API
%% ===================================================================

do_(App, State) ->
    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(Version,
                                          rebar_app_info:dir(App),
                                          rebar_state:resources(State)),
    Type = get_increment(ResolvedVersion),
    do_(Type, App, State).

do_(Type, App, State) ->
    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(Version,
                                          rebar_app_info:dir(App),
                                          rebar_state:resources(State)),
    NewVersion = increment(Type, ec_semver:parse(ResolvedVersion)),
    AppSrcFile = rebar_app_info:app_file_src(App),

    case Version of
        _Git when Version =:= git orelse Version =:= "git" ->
            rebar_api:info("Creating new tag v~s...", [NewVersion]),
            rebar_utils:sh(io_lib:format("git tag v~s", [NewVersion]), []),

            {application, _, AppDetails} = rebar3_hex_utils:update_app_src(App, NewVersion),

            case rebar3_hex_pkg:validate_app_details(AppDetails) of
                ok ->
                    Name = rebar_app_info:name(App),
                    AppDir = rebar_app_info:dir(App),
                    Deps = rebar_state:get(State, {locks, default}, []),
                    TopLevel = [{N, [{<<"app">>, A}, {<<"optional">>, false}, {<<"requirement">>, V}]} || {A,{pkg,N,V,_},0} <- Deps],
                    Excluded = [binary_to_list(N) || {N,{T,_,_},0} <- Deps, T =/= pkg],

                    case rebar3_hex_pkg:publish(AppDir, Name, NewVersion, TopLevel, Excluded, AppDetails) of
                        stopped ->
                            {ok, State};
                        ok ->
                            case ec_talk:ask_default("Push new tag to origin?", boolean, "Y") of
                                true ->
                                    rebar_api:info("Pushing new tag v~s...", [NewVersion]),
                                    rebar_utils:sh(io_lib:format("git push origin v~s", [NewVersion]), []),
                                    {ok, State};
                                false ->
                                    {ok, State}
                            end;
                        Error ->
                            rebar_api:info("Deleting new tag v~s...", [NewVersion]),
                            rebar_utils:sh(io_lib:format("git tag -d v~s", [NewVersion]), []),
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            Spec = rebar3_hex_utils:update_app_src(App, NewVersion),
            NewAppSrcFile = io_lib:format("~tp.\n", [Spec]),
            ok = rebar_file_utils:write_file_if_contents_differ(AppSrcFile, NewAppSrcFile),
            rebar3_hex_pkg:publish(rebar_app_info:original_vsn(App, NewVersion), State),
            ask_commit_and_push(),
            {ok, State}
    end.

get_increment(Version) ->
    ec_talk:say("Select semver increment or other (Current ~s):", [Version]),
    ec_talk:say("1) patch", []),
    ec_talk:say("2) minor", []),
    ec_talk:say("3) major", []),
    ec_talk:say("4) other", []),
    case ec_talk:ask("[1-4] ", number) of
        4 ->
            ec_talk:ask("New Version ", string);
        Type ->
            int_to_bump(Type)
    end.

increment(patch, {{Maj, Min, Patch}, _}) ->
    erlang:iolist_to_binary(ec_semver:format({{Maj, Min, Patch+1}, {[], []}}));
increment(minor, {{Maj, Min, _Patch}, _}) ->
    erlang:iolist_to_binary(ec_semver:format({{Maj, Min+1, 0}, {[], []}}));
increment(major, {{Maj, _Min, _Patch}, _}) ->
    erlang:iolist_to_binary(ec_semver:format({{Maj+1, 0, 0}, {[], []}}));
increment(Version, _) when is_list(Version) ->
    ec_cnv:to_binary(Version).

int_to_bump(1) -> patch;
int_to_bump(2) -> minor;
int_to_bump(3) -> major;
int_to_bump(_) -> error.

string_to_bump("patch") -> patch;
string_to_bump("minor") -> minor;
string_to_bump("major") -> major;
string_to_bump(_) -> error.

ask_commit_and_push() ->
    case ec_talk:ask_default("Create 'version bump' commit?", boolean, "Y") of
        true ->
            rebar_utils:sh("git commit -a -m 'version bump'", []),
            case ec_talk:ask_default("Push master to origin master?", boolean, "N") of
                true ->
                    rebar_utils:sh("git push origin master:master", []);
                false ->
                    ok
            end;
        false ->
            ok
    end.
