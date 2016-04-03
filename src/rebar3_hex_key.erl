-module(rebar3_hex_key).

-export([init/1,
         do/1,
         format_error/1]).

-export([add/3,
         remove/1,
         list/0]).

-include("rebar3_hex.hrl").

-define(PROVIDER, key).
-define(DEPS, []).

-define(ENDPOINT, "keys").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex key [list | remove <key>]"},
                                {short_desc, "Remove or list API keys associated with your account"},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["add", KeyName] ->
            case add(KeyName) of
                {ok, Key} ->
                    ec_talk:say("New API key (~s): ~s", [KeyName, Key]),
                    {ok, State};
                {error, 401, _Response} ->
                    ?PRV_ERROR(401)
            end;
        ["remove", Key] ->
            case remove(Key) of
                ok ->
                    ec_talk:say("Removed API key ~s", [Key]),
                    {ok, State};
                {error, 401} ->
                    ?PRV_ERROR(401);
                {error, 404} ->
                    ?PRV_ERROR(404)
            end;
        ["list"] ->
            case list() of
                {ok, Keys} ->
                    [ec_talk:say("~s", [maps:get(<<"name">>, X)]) || X <- Keys],
                    {ok, State};
                {error, 401} ->
                    ?PRV_ERROR(401)
            end;
        _ ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(401) ->
    "Authentication failed (401)";
format_error(404) ->
    "Not found (404)".

list() ->
    {ok, Auth} = rebar3_hex_config:auth(),
    rebar3_hex_http:get(?ENDPOINT, Auth).

remove(Key) ->
    {ok, Auth} = rebar3_hex_config:auth(),
    rebar3_hex_http:delete(filename:join(?ENDPOINT, Key), Auth).

add(KeyName) ->
    {ok, {Username,Password}} = rebar3_hex_user:ask_password(),
    add(KeyName, Username, Password).
add(KeyName, Username, Password) ->
    Auth = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    rebar_api:info("key=~p", [KeyName]),
    case rebar3_hex_http:post_map("keys", "Basic "++Auth, maps:from_list([{<<"name">>, list_to_binary(KeyName)}])) of
        {ok, Response} ->
            {ok, maps:get(<<"secret">>, Response)};
        {error, Code, Response} ->
            {error, Code, Response}
    end.
