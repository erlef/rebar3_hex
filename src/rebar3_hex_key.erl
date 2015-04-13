-module(rebar3_hex_key).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, key).
-define(DEPS, []).

-define(ENDPOINT, "keys").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, false},
                                {deps, ?DEPS},
                                {example, "rebar3 hex key [list | remove <key>]"},
                                {short_desc, "."},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["remove", Key] ->
            {ok, Auth} = rebar3_hex_config:auth(),
            case rebar3_hex_http:delete(filename:join(?ENDPOINT, Key), Auth) of
                ok ->
                    ok;
                {error, 401} ->
                    rebar_api:error("Authentication failed (401)", [])
            end;
        ["list"] ->
            {ok, Auth} = rebar3_hex_config:auth(),
            case rebar3_hex_http:get(?ENDPOINT, Auth) of
                {ok, Keys} ->
                    [ec_talk:say("~s", [proplists:get_value(<<"name">>, X)]) || X <- Keys];
                {error, 401} ->
                    rebar_api:error("Authentication failed (401)", [])
            end;
        _ ->
            ok
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
