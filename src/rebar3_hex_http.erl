-module(rebar3_hex_http).

-export([put/3
        ,post_json/3
        ,post/3]).

-include("rebar3_hex.hrl").

put(Path, Auth, Body) ->
    case httpc:request(put, json_request(Path, Auth, Body), [], []) of
        {ok, {{_, 200, _}, _, _RespBody}} ->
            ok;
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            RespBody = jsx:decode(list_to_binary(RespBodyJson)),
            {error, RespBody}
    end.

post_json(Path, Auth, Body) ->
    case httpc:request(post, json_request(Path, Auth, Body), [], []) of
        {ok, {{_, 201, _}, _, RespBody}} ->
            {ok, jsx:decode(list_to_binary(RespBody))};
        {ok, {{_, Status, _}, _RespHeaders, _RespBody}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBody}} ->
            {error, RespBody}
    end.

post(Path, Auth, Body) ->
    case httpc:request(post, file_request(Path, Auth, Body), [], [{body_format, binary}]) of
        {ok, {{_, 201, _}, _, _RespBody}} ->
            ok;
        {ok, {{_, Status, _}, _RespHeaders, _RespBodyJson}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            {error, jsx:decode(list_to_binary(RespBodyJson))}
    end.

json_request(Path, Auth, Body) ->
    {binary_to_list(rebar3_hex_config:api_url()) ++ "/api/" ++ Path
    ,[{"authorization", Auth}]
    ,"application/json", jsx:encode(Body)}.

file_request(Path, Auth, Body) ->
    ContentLength = integer_to_list(byte_size(Body)),
    {binary_to_list(rebar3_hex_config:api_url()) ++ "/api/" ++ Path
    ,[{"authorization", Auth}
     ,{"content-length", ContentLength}]
    ,"application/octet-stream", {Body, 0}}.
