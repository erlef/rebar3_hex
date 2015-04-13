-module(rebar3_hex_http).

-export([get/2
        ,delete/2
        ,put/3
        ,post_json/3
        ,post/4]).

-include("rebar3_hex.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-define(ENDPOINT, "/api").

get(Path, Auth) ->
    case httpc:request(get, request(Path, Auth),
                      [{ssl, [ssl_opts(rebar3_hex_config:api_url())]}],
                      [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            {ok, jsx:decode(RespBody)};
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

delete(Path, Auth) ->
    case httpc:request(delete, request(Path, Auth)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, _}} ->
            {error, Status}
    end.

put(Path, Auth, Body) ->
    case httpc:request(put, json_request(Path, Auth, Body)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, _, _}, _, RespBody}} ->
            {error, jsx:decode(RespBody)}
    end.

post_json(Path, Auth, Body) ->
    case httpc:request(post, json_request(Path, Auth, Body)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]) of
        {ok, {{_, Status, _}, _, RespBody}} when Status >= 200, Status =< 299 ->
            {ok, jsx:decode(RespBody)};
        {ok, {{_, Status, _}, _, _}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _, _}, _, RespBody}} ->
            {error, RespBody}
    end.

post(Path, Auth, Body, Size) ->
    case httpc:request(post, file_request(Path, Auth, Body, Size)
                      ,[{ssl, [ssl_opts(rebar3_hex_config:api_url())]}]
                      ,[{body_format, binary}]) of
        {ok, {{_, Status, _}, _, _RespBody}} when Status >= 200, Status =< 299 ->
            ok;
        {ok, {{_, Status, _}, _RespHeaders, _RespBodyJson}} when Status >= 500->
            {error, undefined_server_error};
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            {error, jsx:decode(RespBodyJson)}
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
