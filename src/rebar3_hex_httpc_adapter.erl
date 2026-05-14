%% @private
%% Derived from rebar3_httpc_adapter, which was derived from hex_core v0.7.1 for extra flexibility.

-module(rebar3_hex_httpc_adapter).
-behaviour(hex_http).
-export([request/5, request_to_file/6]).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, ReqHeaders, Body, AdapterConfig) ->
    Profile = maps:get(profile, AdapterConfig, default),
    Request = build_request(URI, ReqHeaders, Body),
    SSLOpts = [{ssl, rebar_utils:ssl_opts(URI)}],
    case httpc:request(Method, Request, SSLOpts, [{body_format, binary}], Profile) of
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} ->
            RespHeaders2 = load_headers(RespHeaders),
            {ok, {StatusCode, RespHeaders2, RespBody}};
        {error, Reason} -> {error, Reason}
    end.

request_to_file(Method, URI, ReqHeaders, Body, Filename, AdapterConfig) ->
    Profile = maps:get(profile, AdapterConfig, default),
    Request = build_request(URI, ReqHeaders, Body),
    SSLOpts = [{ssl, rebar_utils:ssl_opts(URI)}],
    case httpc:request(Method, Request, SSLOpts, [{sync, false}, {stream, self}], Profile) of
        {ok, RequestId} ->
            stream_to_file(RequestId, Filename);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% httpc streams 200/206 responses as messages and returns non-2xx as
%% a normal response tuple. stream_start includes the response headers.
stream_to_file(RequestId, Filename) ->
    receive
        {http, {RequestId, stream_start, Headers}} ->
            {ok, File} = file:open(Filename, [write, binary]),
            case stream_body(RequestId, File) of
                ok ->
                    ok = file:close(File),
                    {ok, {200, load_headers(Headers)}};
                {error, Reason} ->
                    ok = file:close(File),
                    {error, Reason}
            end;
        {http, {RequestId, {{_, StatusCode, _}, RespHeaders, _RespBody}}} ->
            {ok, {StatusCode, load_headers(RespHeaders)}};
        {http, {RequestId, {error, Reason}}} ->
            {error, Reason}
    end.

%% @private
stream_body(RequestId, File) ->
    receive
        {http, {RequestId, stream, BinBodyPart}} ->
            ok = file:write(File, BinBodyPart),
            stream_body(RequestId, File);
        {http, {RequestId, stream_end, _Headers}} ->
            ok;
        {http, {RequestId, {error, Reason}}} ->
            {error, Reason}
    end.

build_request(URI, ReqHeaders, Body) ->
    build_request2(binary_to_list(URI), dump_headers(ReqHeaders), Body).

build_request2(URI, ReqHeaders, undefined) ->
    {URI, ReqHeaders};
build_request2(URI, ReqHeaders, {ContentType, Body}) ->
    {URI, ReqHeaders, ContentType, Body}.

dump_headers(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc] end, [], Map).

load_headers(List) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(list_to_binary(K), list_to_binary(V), Acc) end, #{}, List).

