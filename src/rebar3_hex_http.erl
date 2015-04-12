-module(rebar3_hex_http).

-export([put/3
        ,post_json/3
        ,post/4]).

-include("rebar3_hex.hrl").

-define(ENDPOINT, "/api").

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
        {ok, {{_, Status, _}, _, RespBody}} when Status >= 200, Status =< 299 ->
            {ok, jsx:decode(list_to_binary(RespBody))};
        {ok, {{_, Status, _}, _RespHeaders, _RespBody}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBody}} ->
            {error, RespBody}
    end.

post(Path, Auth, Body, Size) ->
    case httpc:request(post, file_request(Path, Auth, Body, Size), [], []) of
        {ok, {{_, Status, _}, _, _RespBody}} when Status >= 200, Status =< 299 ->
            ok;
        {ok, {{_, Status, _}, _RespHeaders, _RespBodyJson}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            {error, jsx:decode(list_to_binary(RespBodyJson))}
    end.

json_request(Path, Auth, Body) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization",  ec_cnv:to_list(Auth)}]
    ,"application/json", jsx:encode(Body)}.

file_request(Path, Auth, Body, ContentLength) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization", ec_cnv:to_list(Auth)}
     ,{"content-length", ContentLength}]
    ,"application/octet-stream", {Body, 0}}.
