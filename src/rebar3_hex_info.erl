-module(rebar3_hex_info).

-export([init/1
        ,do/1
        ,format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, info).
-define(DEPS, []).

-define(ENDPOINT, "packages").
-define(RELEASE, "releases").
-define(REGISTRY_FILE, "registry").

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
                                {example, "rebar3 hex user <command>"},
                                {short_desc, "Prints hex package or system information"},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        [] ->
            general(State);
        [Package] ->
            package(Package);
        [Package, Version] ->
            release(Package, Version)
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

general(State) ->
    Deps = [case X of
                {Name, Ver} -> {normalize(Name), Ver};
                Name -> {normalize(Name), get_ver(normalize(Name), State)}
            end || X <-rebar_state:get(State, deps, [])],
    display(dedup({Deps, []}), [], State).

%%prints out all deps at level 0, and
%% For those level 0 deps which are package dependencies (Found in Hex Database),
%% it continue printing deps at level > 0
display([], _, _State) ->
    ok;

display([{Name, Vsn} | Deps], Listed, State) when is_list(Vsn) ->
    ec_talk:say("~s ~s", [Name, Vsn]),
    %% accumulates already listed deps
    NewListed = [{Name, Vsn} | Listed],
    case get_deps(Name, Vsn, State) of
        error ->
            display(Deps, NewListed, State);
        NewDeps ->
            display(dedup({Deps ++ NewDeps, NewListed}), NewListed, State)
    end.
%% FIXME: function_clause error for non-hex or {pkg, _ _} deps

%% gets deps name(s) and version(s) for a package
get_deps(Package, Version, State) ->
    case rebar_packages:registry_dir(State) of
        {uri_parse_error, _CDN} ->
            error;
        {ok, RegistryDir} ->
            case ets:file2tab(filename:join(RegistryDir, ?REGISTRY_FILE)) of
                {ok, Registry} ->
                    VerBin = ec_cnv:to_binary(Version),
                    case ets:lookup(Registry, {Package, VerBin}) of
                        [{{Package, VerBin}, [DepList, _, _]}] ->
                            [{normalize(Name), Ver} || [Name, Ver, _ , _] <- DepList];
                        _ -> error
                    end;
                _ ->
                    %% registry table not found
                    case rebar3_hex_http:get(filename:join([?ENDPOINT, Package, "releases", Version]), []) of
                        {ok, Map} ->
                            [{normalize(X), binary_to_list(maps:get(<<"requirement">>, maps:get(X, maps:get(<<"requirements">>, Map))))}
                             || X <- maps:keys(maps:get(<<"requirements">>, Map))];
                        {error, 404} -> error;
                        _ -> error
                    end
            end
    end.

%% gets latest release version
%% returns " " if the Package is not found in Hex
get_ver(Package, State) ->
    RegistryDir = rebar_packages:registry_dir(State),
    case ets:file2tab(filename:join(RegistryDir, ?REGISTRY_FILE)) of
        {ok, Registry} ->
            case ets:lookup(Registry, Package) of
                [{Package, [VerList]}] ->
                    lists:last(VerList);
                _ -> " "
            end;
        _ ->
            %% registry table not found
            case rebar3_hex_http:get(filename:join(?ENDPOINT, Package), []) of
                {ok, Map} ->
                    Release = hd(maps:get(<<"releases">>, Map, [])),
                    binary_to_list(maps:get(<<"version">>, Release, []));
                {error, 404} ->
                    " ";
                _ ->
                    " "
            end
    end.

dedup({Deps, Listed}) ->
    Current = Deps ++ Listed,
    dedup(Deps, [name(Dep) || Dep <- Current]).

dedup([], _) -> [];
dedup([Dep|Deps], [Name|DepNames]) ->
    case lists:member(Name, DepNames) of
        true -> dedup(Deps, DepNames);
        false -> [Dep | dedup(Deps, DepNames)]
    end.

name(T) when is_tuple(T) -> element(1, T).

normalize(Name) when is_binary(Name) ->
    Name;
normalize(Name) when is_atom(Name) ->
    ec_cnv:to_binary(Name).

package(Package) ->
    case rebar3_hex_http:get(filename:join(?ENDPOINT, Package), []) of
        {ok, Map} ->
            ec_talk:say("~s", [Package]),
            Releases = maps:get(<<"releases">>, Map, []),
            ec_talk:say("  Releases: ~s", [string:join([binary_to_list(maps:get(<<"version">>, X, [])) || X <- Releases], ", ")]),
            Meta = maps:get(<<"meta">>, Map),

            Maintainers = maps:get(<<"maintainers">>, Meta, []),
            ec_talk:say("  Maintainers: ~s", [join(Maintainers)]),

            Licenses = maps:get(<<"licenses">>, Meta, []),
            ec_talk:say("  Licenses: ~s", [join(Licenses)]),

            Links = maps:to_list(maps:get(<<"links">>, Meta, [])),
            ec_talk:say("  Links:\n    ~s", [tup_list_join(Links)]),

            Description = maps:get(<<"description">>, Meta, []),
            ec_talk:say(Description, []);
        {error, 404} ->
            rebar_api:error("No package with name ~s", [Package]);
        _ ->
            rebar_api:error("Failed to retrieve package information")
    end.


release(Package, Version) ->
    case rebar3_hex_http:get(filename:join([?ENDPOINT, Package, "releases", Version]), []) of
        {ok, Map} ->
            ec_talk:say("~s ~s", [Package, Version]),
            ec_talk:say("  Dependencies:\n    ~s", [req_join(Map)]);
        {error, 404} ->
            rebar_api:error("No package with name ~s", [Package]);
        _ ->
            rebar_api:error("Failed to retrieve package information")
    end.

join(List) ->
    string:join([binary_to_list(X) || X <- List], ", ").

tup_list_join(List) ->
    string:join([binary_to_list(X)++": "++binary_to_list(Y) || {X, Y} <- List], "\n    ").

req_join(ReleaseMap) ->
    string:join([binary_to_list(X)++": "++binary_to_list(maps:get(<<"requirement">>, maps:get(X, maps:get(<<"requirements">>, ReleaseMap))))
                || X <- maps:keys(maps:get(<<"requirements">>, ReleaseMap))],"\n    ").
