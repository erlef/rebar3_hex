-module(rebar3_hex_info).

-export([init/1
        ,do/1
        ,format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, info).
-define(DEPS, []).

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
            general();
        [Package] ->
            package(Package);
        [Package, Version] ->
            release(Package, Version)
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

general() ->
    ok.

package(Package) ->
    case rebar3_hex_http:get(filename:join(?ENDPOINT, Package), []) of
        {ok, Map} ->
            ec_talk:say("~s", [Package]),
            Releases = maps:get(<<"releases">>, Map, []),
            ec_talk:say("  Releases: ~s", [string:join([binary_to_list(maps:get(<<"version">>, X, [])) || X <- Releases], ", ")]),
            Meta = maps:get(<<"meta">>, Map),

            % Remove this when Hex no longer supports contributors
            Contributors = maps:get(<<"contributors">>, Meta, []),
            ec_talk:say("  Contributors: ~s", [join(Contributors)]),

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
