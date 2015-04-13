-module(rebar3_hex_http).

-export([get/2
        ,delete/2
        ,put/3
        ,post_json/3
        ,post/4]).

-include("rebar3_hex.hrl").

-define(ENDPOINT, "/api").

get(Path, Auth) ->
    case httpc:request(get, request(Path, Auth), [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, jsx:decode(RespBody)};
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

delete(Path, Auth) ->
    case httpc:request(delete, request(Path, Auth), [], [{body_format, binary}]) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

put(Path, Auth, Body) ->
    case httpc:request(put, json_request(Path, Auth, Body), [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, _, _}, _, RespBody}} ->
            {error, jsx:decode(RespBody)}
    end.

post_json(Path, Auth, Body) ->
    case httpc:request(post, json_request(Path, Auth, Body), [], [{body_format, binary}]) of
        {ok, {{_, Status, _}, _, RespBody}} when Status >= 200, Status =< 299 ->
            {ok, jsx:decode(RespBody)};
        {ok, {{_, Status, _}, _, _}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _, _}, _, RespBody}} ->
            {error, RespBody}
    end.

post(Path, Auth, Body, Size) ->
    case httpc:request(post, file_request(Path, Auth, Body, Size), [], [{body_format, binary}]) of
        {ok, {{_, Status, _}, _, _RespBody}} when Status >= 200, Status =< 299 ->
            ok;
        {ok, {{_, Status, _}, _RespHeaders, _RespBodyJson}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            {error, jsx:decode(RespBodyJson)}
    end.

request(Path, Auth) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization",  ec_cnv:to_list(Auth)}]}.

json_request(Path, Auth, Body) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization",  ec_cnv:to_list(Auth)}]
    ,"application/json", jsx:encode(Body)}.

file_request(Path, Auth, Body, ContentLength) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization", ec_cnv:to_list(Auth)}
     ,{"content-length", ContentLength}]
    ,"application/octet-stream", {Body, 0}}.
