-module(rebar3_hex_info).

-behaviour(provider).

-export([init/1
        ,do/1
        ,format_error/1]).

-include("rebar3_hex.hrl").
-include_lib("providers/include/providers.hrl").

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
                                {bare, false},
                                {deps, ?DEPS},
                                {example, "rebar3 hex user <command>"},
                                {short_desc, "."},
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
        {ok, Json} ->
            ec_talk:say("~s", [Package]),
            Releases = proplists:get_value(<<"releases">>, Json, []),
            ec_talk:say("  Releases: ~s", [string:join([binary_to_list(proplists:get_value(<<"version">>, X)) || X <- Releases], ", ")]),
            Meta = proplists:get_value(<<"meta">>, Json, []),

            Contributors = proplists:get_value(<<"contributors">>, Meta, []),
            ec_talk:say("  Contributors: ~s", [join(Contributors)]),

            Licenses = proplists:get_value(<<"licenses">>, Meta, []),
            ec_talk:say("  Licenses: ~s", [join(Licenses)]),

            Links = proplists:get_value(<<"links">>, Meta, []),
            ec_talk:say("  Links:\n    ~s", [tup_list_join(Links)]),

            Description = proplists:get_value(<<"description">>, Meta, []),
            ec_talk:say(Description, []);
        {error, 404} ->
            rebar_api:error("No package with name ~s", [Package]);
        _ ->
            rebar_api:error("Failed to retrieve package information")
    end.

release(Package, Version) ->
    case rebar3_hex_http:get(filename:join([?ENDPOINT, Package, "releases", Version]), []) of
        {ok, Json} ->
            ec_talk:say("~s ~s", [Package, Version]),
            Requirements = proplists:get_value(<<"requirements">>, Json, []),
            ec_talk:say("  Dependencies:\n    ~s", [req_join(Requirements)]);
        {error, 404} ->
            rebar_api:error("No package with name ~s", [Package]);
        _ ->
            rebar_api:error("Failed to retrieve package information")
    end.

join(List) ->
    string:join([binary_to_list(X) || X <- List], ", ").

tup_list_join(List) ->
    string:join([binary_to_list(X)++": "++binary_to_list(Y) || {X, Y} <- List], "\n    ").

req_join(List) ->
    string:join([binary_to_list(X)++": "++binary_to_list(proplists:get_value(<<"requirement">>, Y)) || {X, Y} <- List], "\n    ").
