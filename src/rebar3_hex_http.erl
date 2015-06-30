-module(rebar3_hex_http).

-export([get/2
        ,delete/2
        ,put/3
        ,post_json/3
        ,post/4
        ,encode/1
        ,maybe_setup_proxy/0]).

-include("rebar3_hex.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-define(ENDPOINT, "/api").

get(Path, Auth) ->
    case httpc:request(get, request(Path, Auth)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, jsx:decode(RespBody)};
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

delete(Path, Auth) ->
    case httpc:request(delete, request(Path, Auth)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

put(Path, Auth, Body) ->
    case httpc:request(put, json_request(Path, Auth, Body)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, Status, _}, _, _}} when Status >= 200, Status =< 299  ->
            ok;
        {ok, {{_, _, _}, _, RespBody}} ->
            {error, RespBody}
    end.

post_json(Path, Auth, Body) ->
    case httpc:request(post, json_request(Path, Auth, Body)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, Status, _}, _, RespBody}} when Status >= 200, Status =< 299 ->
            {ok, jsx:decode(RespBody)};
        {ok, {{_, Status, _}, _, _}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _, _}, _, RespBody}} ->
            {error, RespBody}
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
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]
                      ,hex) of
        {ok, {{_, Status, _}, _, _RespBody}} when Status >= 200, Status =< 299 ->
            ok;
        {ok, {{_, Status, _}, _RespHeaders, _RespBodyJson}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, Status, _}, _RespHeaders, RespBodyJson}} ->
            {error, Status, jsx:decode(RespBodyJson)}
    end.

%% Internal Functions

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

ssl_opts(Url) ->
    {ok, {_, _, Hostname, _, _, _}} = http_uri:parse(ec_cnv:to_list(Url)),
    VerifyFun = {fun ssl_verify_hostname:verify_fun/3, [{check_hostname, Hostname}]},
    CACerts = cacerts(),
    [{verify, verify_peer}, {depth, 2}, {cacerts, CACerts}
    ,{partial_chain, fun partial_chain/1}, {verify_fun, VerifyFun}].

maybe_setup_proxy() ->
    maybe_setup_proxy(https_proxy, rebar3_hex_config:https_proxy()),
    maybe_setup_proxy(proxy, rebar3_hex_config:http_proxy()).

maybe_setup_proxy(_, []) ->
    ok;
maybe_setup_proxy(Scheme, Proxy) ->
    {ok, {_, _, Host, Port, _, _}} = http_uri:parse(Proxy),
    httpc:set_options([{Scheme, {{Host, Port}, []}}], hex).

partial_chain(Certs) ->
    Certs = [{Cert, public_key:pkix_decode_cert(Cert, otp)} || Cert <- Certs],
    CACerts = cacerts(),
    CACerts1 = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CACerts],

    case ec_lists:find(fun({Der, Cert}) ->
                              check_cert(CACerts1, Der, Cert)
                       end, Certs) of
        {ok, Trusted} ->
            {trusted_ca, Trusted};
        _ ->
            unknown_ca
    end.

extract_key(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo)#'OTPSubjectPublicKeyInfo'.subjectPublicKey.

cacerts() ->
    Pems = public_key:pem_decode(rebar3_hex_cacerts:cacerts()),
    [Der || {'Certificate', Der, _} <- Pems].

check_cert(CACerts, Der, Cert) ->
    lists:any(fun(CACert) ->
                      case public_key:pkix_is_issuer(Cert, CACert) of
                          true ->
                              Key = extract_key(CACert),
                              public_key:pkix_verify(Der, Key);
                          _ ->
                              false
                      end
              end, CACerts).

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
