%%% @doc: A hex.pm API model
%%%

-module(hex_api_model).
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

handle('POST', [<<"packages">>, _Name, <<"releases">>, _Version, <<"docs">>], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
           respond_with(201, Req, <<>>);
       error ->
           respond_with(401, Req, #{})
   end;

handle('GET', [<<"packages">>, _Name, <<"owners">>], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
            respond_with(200, Req, [#{}]);
       error ->
           respond_with(401, Req, #{})
   end;

handle('DELETE', [<<"packages">>, _Name, <<"owners">>, _UserOrOrg], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
        {204, [{<<"Foo">>, <<"Bar">>}], terms_to_body(Req, <<"">>)};
       error ->
           respond_with(401, Req, #{})
   end;

handle('PUT', [<<"packages">>, _Name, <<"owners">>, _UserOrOrg], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
           respond_with(201, Req, <<>>);
       error ->
           respond_with(401, Req, #{})
   end;

handle('DELETE', [<<"packages">>, _Name, <<"releases">>, _Version], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
           {204, [{<<"Foo">>, <<"Bar">>}], terms_to_body(Req, <<"">>)};
       error ->
           respond_with(401, Req, #{})
   end;

handle('DELETE', [<<"packages">>, _Name, <<"releases">>, _Version, <<"docs">>], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
           {204, [{<<"Foo">>, <<"Bar">>}], terms_to_body(Req, <<"">>)};
       error ->
           respond_with(401, Req, #{})
   end;

handle('POST', [<<"publish">>], Req) ->
    case authenticate(Req) of
       {ok, #{username := Username, email := Email}} ->
           {ok, Meta, _Checksum}     = body_to_meta(Req),
           App = maps:get(<<"app">>, Meta),

           Res = #{
             <<"version">> => maps:get(<<"version">>, Meta),
             <<"has_docs">> => false,
             <<"downloads">> => undefined,
             <<"inserted_at">> => timestamp(),
             <<"updated_at">> => timestamp(),
             <<"retirement">> => undefined,
             <<"package_url">> => <<?BASE_USER_URL/bitstring, App/bitstring>>,
             <<"html_url">> => <<?BASE_USER_URL/bitstring, App/bitstring>>,
             <<"docs_html_url">> => undefined,
             <<"requirements">> => maps:get(<<"requirements">>, Meta),
             <<"meta">> => #{
                 <<"app">> => App,
                 <<"build_tools">> => maps:get(<<"build_tools">>, Meta),
                 <<"elixir">> => undefined
                },
             <<"publisher">> => #{
                 <<"email">> => Email,
                 <<"url">> => <<?BASE_USER_URL/bitstring, Username/bitstring>>,
                 <<"username">> => Username
                }
            },
           respond_with(201, Req, Res);
       unauthorized ->
            respond_with(403, Req, #{<<"message">> => <<"account not authorized for this action">>});
       error ->
           respond_with(401, Req, #{})
   end;

handle('POST', [<<"repos">>, _Repo, <<"publish">>], Req) ->
    case authenticate(Req) of
       {ok, #{username := Username, email := Email}} ->
            {ok, Meta, _Checksum}     = body_to_meta(Req),
            App = maps:get(<<"app">>, Meta),

           Res = #{
             <<"version">> => maps:get(<<"version">>, Meta),
             <<"has_docs">> => false,
             <<"downloads">> => undefined,
             <<"inserted_at">> => timestamp(),
             <<"updated_at">> => timestamp(),
             <<"retirement">> => undefined,
             <<"package_url">> => <<?BASE_USER_URL/bitstring, App/bitstring>>,
             <<"html_url">> => <<?BASE_USER_URL/bitstring, App/bitstring>>,
             <<"docs_html_url">> => undefined,
             <<"requirements">> => maps:get(<<"requirements">>, Meta),
             <<"meta">> => #{
                 <<"app">> => App,
                 <<"build_tools">> => maps:get(<<"build_tools">>, Meta),
                 <<"elixir">> => undefined
                },
             <<"publisher">> => #{
                 <<"email">> => Email,
                 <<"url">> => <<?BASE_USER_URL/bitstring, Username/bitstring>>,
                 <<"username">> => Username
                }
            },
           respond_with(201, Req, Res);
       unauthorized ->
            respond_with(403, Req, #{<<"message">> => <<"account not authorized for this action">>});
        error ->
           respond_with(401, Req, #{})
   end;

handle('POST', [<<"keys">>], Req) ->
    Data     = body_to_terms(Req),
    _Name = maps:get(<<"name">>, Data),
    _Perms    = maps:get(<<"permissions">>, Data),
    Res = #{
      <<"secret">> => <<"repo_key">>
    },
    respond_with(201, Req, Res);

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

handle('POST', [<<"users">>, User, <<"reset">>], Req) ->
    {Status, Res} = case User of
                        <<"eh%3F">> ->
                            {422, #{<<"message">> => <<"huh?">>}};
                        <<"bad">> ->
                            {500, #{<<"whoa">> => <<"mr.">>}};
                        _Other ->
                            {204, <<>>}
                    end,
        case Status of
            204 ->
                {Status, [{<<"Foo">>, <<"Bar">>}], terms_to_body(Req, Res)};
            _ ->
                respond_with(Status, Req, Res)
        end;

handle('GET', [<<"users">>, <<"me">>], Req) ->
    Key = elli_request:get_header(<<"Authorization">>, Req),
    {Status, Res} = case Key of
                        <<"eh?">> ->
                            {422, #{<<"message">> => <<"huh?">>}};
                        <<"bad">> ->
                            {500, #{<<"whoa">> => <<"mr.">>}};
                        _Other ->
                            Res0 = #{<<"username">> => <<"mr_pockets">>,
                                     <<"email">> => <<"foo@bar.baz">>
                                    },
                            {200, Res0}
                    end,
    respond_with(Status, Req, Res);

handle('GET', [<<"keys">>], Req) ->
    Key = elli_request:get_header(<<"Authorization">>, Req),
    {Status, Res} = case Key of
                        <<"eh?">> ->
                            {422, #{<<"message">> => <<"huh?">>}};
                        <<"bad">> ->
                            {500, #{<<"whoa">> => <<"mr.">>}};
                        _Other ->
                            Res0 = [
                                    #{<<"name">> => <<"key1">>,
                                     <<"inserted_at">> => <<"2019-05-27T20:49:35Z">>
                                    },
                                    #{<<"name">> => <<"key2">>,
                                     <<"inserted_at">> => <<"2019-06-27T20:49:35Z">>
                                    }
                                   ],
                            {200, Res0}
                    end,
    respond_with(Status, Req, Res);

handle('GET', [<<"keys">>, <<"key">>], Req) ->
    Res = #{
            <<"authing_key">> => false,
            <<"inserted_at">> => <<"2019-03-23T17:47:35Z">>,
            <<"last_use">>=> #{
                                <<"ip">> => <<"173.166.199.194">>,
                                <<"used_at">> => <<"2019-04-16T03:11:05Z">>,
                                <<"user_agent">> => <<"hex_core/0.5.0 (rebar3/3.9.1+build.4339.refd29728e) (httpc) (OTP/21) (erts/10.2.5)">>
                                },
            <<"name">>=><<"mr_pockets_house">>,
            <<"permissions">>=>[
                                #{
                                    <<"domain">>=><<"api">>,
                                    <<"resource">>=>nil
                                }
                               ],
            <<"revoked_at">>=>nil,
            <<"secret">>=>nil,
            <<"updated_at">>=><<"2019-04-16T03:11:05Z">>,
            <<"url">>=><<"https://hex.pm/api/keys/mr_pockets_house">>
     },
    respond_with(200, Req, Res);

handle('PUT', [<<"keys">>], Req) ->
    Res = #{},
    respond_with(201, Req, Res);


handle('DELETE', [<<"keys">>], Req) ->
    Res = #{},
    respond_with(200, Req, Res);

handle('DELETE', [<<"keys">>, <<"key">>], Req) ->
    Res = #{<<"secret">> => <<"repo_key">>},
    respond_with(200, Req, Res);

handle(_, _Path, Req) ->
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

body_to_meta(Req) ->
    Body = elli_request:body(Req),
    case hex_tarball:unpack(Body, memory) of
        {ok, #{inner_checksum := Checksum, metadata :=  Metadata}} ->
            {ok, Metadata, Checksum};
        {error, Reason} ->
            {error, list_to_bitstring(hex_tarball:format_error(Reason))}
    end.

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

authenticate(Req) ->
    case elli_request:get_header(<<"Authorization">>, Req) of
        Key when Key =:= <<"key">> orelse Key =:= <<"123">> ->
            {ok, #{username => <<"mr_pockets">>,
                   email => <<"foo@bar.baz">>,
                   organization => <<"hexpm">>,
                   source => key,
                   key => <<"key">>}};
        <<"unauthorized">> ->
            unauthorized;
        _ ->
            error
    end.
