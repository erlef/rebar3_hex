-module(rebar3_hex_client).

-export([key_add/3, key_get/2, key_delete/2, key_delete_all/1, key_list/1]).

-include("rebar3_hex.hrl").

key_add(HexConfig, <<KeyName/binary>>, Perms) ->
    Res = hex_api_key:add(HexConfig, KeyName, Perms),
    response(Res);
key_add(HexConfig, KeyName, Perms) ->
    key_add(HexConfig, to_binary(KeyName), Perms).

key_get(HexConfig, <<KeyName/binary>>) ->
    Res = hex_api_key:get(HexConfig, KeyName),
    response(Res);
key_get(HexConfig, KeyName) ->
    key_get(HexConfig, to_binary(KeyName)).

key_list(HexConfig) ->
    Res = hex_api_key:list(HexConfig),
    response(Res).

key_delete(HexConfig, <<KeyName/binary>>) ->
    Res = hex_api_key:delete(HexConfig, KeyName),
    response(Res);
key_delete(HexConfig, KeyName) ->
    key_delete(HexConfig, to_binary(KeyName)).

key_delete_all(HexConfig) ->
    Res = hex_api_key:delete_all(HexConfig),
    response(Res).

response({ok, {200, _Headers, Res}}) ->
    {success, Res};
response({ok, {201, _Headers, Res}}) ->
    {created, Res};
response({ok, {204, _Headers, Res}}) ->
    {success, Res};
response({ok, {401, _Headers, Res}}) ->
    {unauthorized, Res};
response({ok, {404, _Headers, Res}}) ->
    {not_found, Res};
response({ok, {422, _Headers, #{<<"message">> := <<"Validation error(s)">>} = Res}}) ->
    {validation_errors, Res};
response({ok, {422, _Headers, Res}}) ->
    {unprocessable, Res};
response({_, _} = Unknown) ->
    Unknown.

to_binary(Subject) ->
    rebar_utils:to_binary(Subject).
