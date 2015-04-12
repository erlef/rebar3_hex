-module(rebar3_hex_http).

-export([put/3
        ,post_json/3
        ,post/3]).

-include("rebar3_hex.hrl").

put(Url, Auth, Body) ->
    case httpc:request(put, {rebar3_hex_config:api_url()++"/api/"++Url, [{"authorization", Auth}], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, _RespBody}} ->
            ok;
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            RespBody = jsx:decode(list_to_binary(RespBodyJson)),
            {error, RespBody}
    end.

post_json(Url, Auth, Body) ->
    ContentLength = integer_to_list(byte_size(Body)),
    case httpc:request(post, {rebar3_hex_config:api_url()++"/api/"++Url, [{"authorization", Auth}
                                   ,{"content-length", ContentLength}]
                             ,"application/json", Body}, [], []) of
        {ok, {{_, 201, _}, _, RespBody}} ->
            {ok, jsx:decode(list_to_binary(RespBody))};
        {ok, {{_, Status, _}, _RespHeaders, _RespBodyJson}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            {error, jsx:decode(list_to_binary(RespBodyJson))}
    end.

post(Url, Auth, Body) ->
    ContentLength = integer_to_list(byte_size(Body)),
    case httpc:request(post, {rebar3_hex_config:api_url()++"/api/"++Url, [{"authorization", Auth}
                                   ,{"content-length", ContentLength}]
                             ,"application/octet-stream", {Body, 0}}, [], [{body_format, binary}]) of
        {ok, {{_, 201, _}, _, _RespBody}} ->
            ok;
        {ok, {{_, Status, _}, _RespHeaders, _RespBodyJson}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            {error, jsx:decode(list_to_binary(RespBodyJson))}
    end.
