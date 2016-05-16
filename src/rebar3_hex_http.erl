-module(rebar3_hex_http).

-export([get/2
        ,delete/2
        ,put/3
        ,post_map/3
        ,post/4
        ,encode/1
        ,pretty_print_status/1
        ,maybe_setup_proxy/0
        ,user_agent/0]).

-include("rebar3_hex.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-define(ENDPOINT, "/api").

get(Path, Auth) ->
    case httpc:request(get, request(Path, Auth)
                      ,[{ssl, rebar_api:ssl_opts(rebar3_hex_config:api_url())}, {relaxed, true}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, 200, _}, _, RespBody}} ->
        {ok, binary_to_term(RespBody)};
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

delete(Path, Auth) ->
    case httpc:request(delete, request(Path, Auth)
                      ,[{ssl, rebar_api:ssl_opts(rebar3_hex_config:api_url())}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

put(Path, Auth, Body) ->
    case httpc:request(put, map_request(Path, Auth, Body)
                      ,[{ssl, rebar_api:ssl_opts(rebar3_hex_config:api_url())}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, Status, _}, _, _}} when Status >= 200, Status =< 299  ->
            ok;
        {ok, {{_, Status, _}, _, RespBody}} ->
            {error, Status, RespBody}
    end.

post_map(Path, Auth, Body) ->
    case httpc:request(post, map_request(Path, Auth, Body)
                      ,[{ssl, rebar_api:ssl_opts(rebar3_hex_config:api_url())}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, 204, _}, _, <<>>}} ->
            ok;
        {ok, {{_, Status, _}, _, RespBody}} when Status >= 200, Status =< 299 ->
            {ok, binary_to_term(RespBody)};
        {ok, {{_, Status, _}, _, _}} when Status >= 500->
            {error, Status, undefined_server_error};
        {ok, {{_, Status, _}, _, RespBody}} ->
            {error, Status, binary_to_term(RespBody)}
    end.

post(Path, Auth, Tar, Size) ->
    Body = fun(S) when S < byte_size(Tar) ->
                   NewSize = min(S + ?CHUNK, byte_size(Tar)),
                   Chunk = NewSize - S,
                   {ok, [binary:part(Tar, S, Chunk)], NewSize};
              (_S) ->
                   eof
           end,

    case httpc:request(post, file_request(Path, Auth, Body, Size)
                      ,[{ssl, rebar_api:ssl_opts(rebar3_hex_config:api_url())}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, Status, _}, _, _RespBody}} when Status >= 200, Status =< 299 ->
            ok;
    {ok, {{_, Status, _}, _RespHeaders, _RespBodyMap}} when Status >= 500->
            io:format("~p ~p~n", [_RespHeaders, _RespBodyMap]),
            {error, Status, undefined_server_error};
    {ok, {{_, Status, _}, _RespHeaders, RespBodyMap}} ->
            {error, Status, binary_to_term(RespBodyMap)}
    end.

pretty_print_status(401) -> "Authentication failed (401)";
pretty_print_status(403) -> "Forbidden (403)";
pretty_print_status(404) -> "Entity not found (404)";
pretty_print_status(422) -> "Validation failed (422)";
pretty_print_status(Code) -> io_lib:format("HTTP status code: ~p", [Code]).

%% Internal Functions

request(Path, Auth) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization",  ec_cnv:to_list(Auth)}, {"user-agent", user_agent()}
     , {"Accept", "application/vnd.hex+erlang"}]}.

map_request(Path, Auth, Body) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization",  ec_cnv:to_list(Auth)}, {"user-agent", user_agent()},
      {"Accept", "application/vnd.hex+erlang"}]
    ,"application/vnd.hex+erlang", term_to_binary(Body)}.


file_request(Path, Auth, Body, ContentLength) ->
    {ec_cnv:to_list(rebar3_hex_config:api_url()) ++ ec_cnv:to_list(filename:join(?ENDPOINT, Path))
    ,[{"authorization", ec_cnv:to_list(Auth)}
     ,{"content-length", ContentLength}, {"user-agent", user_agent()},
      {"Accept", "application/vnd.hex+erlang"}] %%Erlang media type
    ,"application/octet-stream", {Body, 0}}.

maybe_setup_proxy() ->
    maybe_setup_proxy(https_proxy, rebar3_hex_config:https_proxy()),
    maybe_setup_proxy(proxy, rebar3_hex_config:http_proxy()).

maybe_setup_proxy(_, []) ->
    ok;
maybe_setup_proxy(Scheme, Proxy) ->
    {ok, {_, _, Host, Port, _, _}} = http_uri:parse(Proxy),
    httpc:set_options([{Scheme, {{Host, Port}, []}}], hex).

encode(Term) ->
    quote_plus(Term).

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
            (C >= $a andalso C =< $f) orelse
            (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
             (C >= $A andalso C =< $Z) orelse
             (C >= $0 andalso C =< $9) orelse
             (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
              C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

%% @spec quote_plus(atom() | integer() | string()) -> string()
%% @doc URL safe encoding of the given term.
quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

user_agent() ->
    application:load(rebar3_hex),
    {ok, RebarVsn} = application:get_key(rebar, vsn),
    {ok, RebarHexVsn} = application:get_key(rebar3_hex, vsn),
    io_lib:format("rebar3_hex/~s (rebar3/~s Erlang/~s)", [RebarHexVsn
                                                         ,RebarVsn
                                                         ,rebar_utils:otp_release()]).
