%%% @doc: A mock hex.pm API 
%%%

-module(hex_api_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(MIME_TYPES, #{
            json                              => <<"application/json">>,
            hex_json                              => <<"application/vnd.hex+json">>,
            erlang                            => <<"application/vnd.hex+erlang">>,
            <<"application/json">>            => json,
            <<"application/vnd.hex+json">>    => hex_json,
            <<"application/vnd.hex+erlang">>  => erlang
         }).

-define(JSON_MIME, {<<"Content-type">>, <<"application/json; charset=ISO-8859-1">>}).
-define(BASE_USER_URL, <<"https://127.0.0.1:3000/api/users/">>).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', [<<"users">>], Req) ->
    Data     = body_to_terms(Req),
    Username = maps:get(<<"username">>, Data),
    Email    = maps:get(<<"email">>, Data),
    _Passwd  = maps:get(<<"password">>, Data),
    Ts = timestamp(),
    Res = #{
      <<"username">> => Username,
      <<"email">> => Email,
      <<"inserted_at">> => Ts,
      <<"updated_at">> => Ts,
      <<"url">> => <<?BASE_USER_URL/bitstring, Username/bitstring>>
    },
  case hex_db:add_user(Res) of
      {ok, Username} -> 
        respond_with(201, Req, Res);
      {error, user_exists} ->
          ErrVal = <<"has already been taken">>,
          respond_with(422, Req, #{<<"errors">> => #{<<"username">> => ErrVal}, 
                                   <<"status">> => 422, 
                                   <<"message">> => <<"validation failed">>});
     {error, email_exists} ->
          ErrVal = <<"already in use">>,
          respond_with(422, Req, #{<<"errors">> => #{<<"email">> => ErrVal}, 
                                   <<"status">> => 422, 
                                   <<"message">> => <<"validation failed">>})
  end;
handle(_, _, Req) ->
    respond_with(404, Req, #{}). 

handle_event(_Event, _Data, _Args) ->
    ok.

%% Helpers
timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    Format = "~b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b",
    io_lib:format(Format, [Y, M, D, H, Mi, S]).

respond_with(Status, Req, Data) ->
      {Status, [{<<"Content-type">>, send_as(Req)}], terms_to_body(Req, Data)}.

client_accepts(Req) ->
    maps:get(elli_request:get_header(<<"Accept">>, Req), ?MIME_TYPES).

send_as(Req) ->
    maps:get(client_accepts(Req), ?MIME_TYPES).

body_to_terms(Req)  ->
    Body = elli_request:body(Req),
    from(client_accepts(Req), Body).

terms_to_body(Req, Data)  ->
    to(client_accepts(Req), Data).

from(erlang, Bin) ->
    binary_to_term(Bin);
from(T, Str) when T =:= json andalso t =:= hex_json ->
    jsone:decode(Str).

to(erlang, Term) ->
    term_to_binary(Term);
to(T, Term) when T =:= json andalso T =:= hex_json ->
     jsone:encode(Term).
