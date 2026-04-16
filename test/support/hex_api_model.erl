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

-define(BASE_USER_URL, <<"https://127.0.0.1:3000/api/users/">>).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"auth">>], Req) ->
    respond_with(200, Req, #{});

%% OAuth Device Authorization Flow endpoints
handle('POST', [<<"oauth">>, <<"device_authorization">>], Req) ->
    %% Check if we should simulate an error
    case hex_db:get_oauth_device(<<"force_error">>) of
        error ->
            respond_with(401, Req, #{<<"message">> => <<"unauthorized">>});
        _ ->
            %% Store the device code in hex_db so we can track it
            DeviceCode = <<"test_device_code_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            UserCode = <<"TEST-CODE">>,
            hex_db:set_oauth_device(DeviceCode, pending),
            Res = #{
                <<"device_code">> => DeviceCode,
                <<"user_code">> => UserCode,
                <<"verification_uri">> => <<"http://127.0.0.1:3000/oauth/verify">>,
                <<"verification_uri_complete">> => <<"http://127.0.0.1:3000/oauth/verify?code=", UserCode/binary>>,
                <<"expires_in">> => 900,
                <<"interval">> => 1
            },
            respond_with(200, Req, Res)
    end;

handle('POST', [<<"oauth">>, <<"token">>], Req) ->
    Data = body_to_terms(Req),
    case maps:get(<<"grant_type">>, Data, undefined) of
        <<"urn:ietf:params:oauth:grant-type:device_code">> ->
            DeviceCode = maps:get(<<"device_code">>, Data),
            case hex_db:get_oauth_device(DeviceCode) of
                authorized ->
                    %% User has authorized - return tokens
                    ExpiresAt = erlang:system_time(second) + 3600,
                    Res = #{
                        <<"access_token">> => <<"test_access_token">>,
                        <<"refresh_token">> => <<"test_refresh_token">>,
                        <<"token_type">> => <<"bearer">>,
                        <<"expires_in">> => 3600,
                        <<"expires_at">> => ExpiresAt
                    },
                    respond_with(200, Req, Res);
                pending ->
                    %% Auto-authorize on first poll for testing convenience
                    hex_db:set_oauth_device(DeviceCode, authorized),
                    ExpiresAt = erlang:system_time(second) + 3600,
                    Res = #{
                        <<"access_token">> => <<"test_access_token">>,
                        <<"refresh_token">> => <<"test_refresh_token">>,
                        <<"token_type">> => <<"bearer">>,
                        <<"expires_in">> => 3600,
                        <<"expires_at">> => ExpiresAt
                    },
                    respond_with(200, Req, Res);
                undefined ->
                    respond_with(400, Req, #{<<"error">> => <<"invalid_grant">>})
            end;
        <<"refresh_token">> ->
            %% Handle token refresh
            ExpiresAt = erlang:system_time(second) + 3600,
            Res = #{
                <<"access_token">> => <<"refreshed_access_token">>,
                <<"refresh_token">> => <<"refreshed_refresh_token">>,
                <<"token_type">> => <<"bearer">>,
                <<"expires_in">> => 3600,
                <<"expires_at">> => ExpiresAt
            },
            respond_with(200, Req, Res);
        _ ->
            respond_with(400, Req, #{<<"error">> => <<"unsupported_grant_type">>})
    end;

handle('POST', [<<"oauth">>, <<"revoke">>], Req) ->
    %% Token revocation - always succeeds
    respond_with(200, Req, #{});

%% Endpoint to simulate user authorizing the device (for tests to call)
handle('POST', [<<"oauth">>, <<"authorize_device">>], Req) ->
    Data = body_to_terms(Req),
    DeviceCode = maps:get(<<"device_code">>, Data),
    hex_db:set_oauth_device(DeviceCode, authorized),
    respond_with(200, Req, #{<<"status">> => <<"authorized">>});

handle('GET', [<<"orgs">>, <<"foo">>, <<"keys">>], Req) ->
    Res = [
            #{<<"name">> => <<"key1">>,
              <<"inserted_at">> => <<"2019-05-27T20:49:35Z">>
             },
            #{<<"name">> => <<"key2">>,
              <<"inserted_at">> => <<"2019-06-27T20:49:35Z">>
             }
          ],
    respond_with(200, Req, Res);


handle('POST', [<<"orgs">>, <<"foo">>, <<"keys">>], Req) ->
    Res = #{
      <<"secret">> => <<"repo_key">>
    },
    respond_with(201, Req, Res);

handle('DELETE', [<<"orgs">>, <<"foo">>, <<"keys">>], Req) ->
    respond_with(201, Req, #{});

handle('DELETE', [<<"orgs">>, <<"foo">>, <<"keys">>, <<"this-key">>], Req) ->
    respond_with(201, Req, #{});

handle('POST', [<<"packages">>, _Name, <<"releases">>, _Version, <<"docs">>], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
           respond_with(201, Req, <<>>);
       error ->
           respond_with(401, Req, #{})
   end;

handle('POST', [<<"repos">>, _Repo, <<"packages">>, _Name, <<"releases">>, _Version, <<"docs">>], Req) ->
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
    handle_publish(Req);

handle('POST', [<<"packages">>, _Name, <<"releases">>], Req) ->
    handle_publish(Req);

handle('POST', [<<"repos">>, _Repo, <<"publish">>], Req) ->
    handle_publish(Req);

handle('POST', [<<"repos">>, _Repo, <<"packages">>, _Name, <<"releases">>], Req) ->
    handle_publish(Req);

handle('POST', [<<"packages">>,_Name,<<"releases">>,_Ver,<<"retire">>], Req) ->
    case authenticate(Req) of
       {ok, #{username := _Username, email := _Email}} ->
            {204, [{<<"Content-type">>, "text/plain"}], <<>>};
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
    Passwd  = maps:get(<<"password">>, Data),
    Ts = timestamp(),
    Res = #{
      <<"username">> => Username,
      <<"email">> => Email,
      <<"inserted_at">> => Ts,
      <<"updated_at">> => Ts,
      <<"password">> => Passwd,
      <<"url">> => <<?BASE_USER_URL/bitstring, Username/bitstring>>
    },
  case hex_db:add_user(Res) of
      {ok, _Username} ->
        respond_with(201, Req, Res);
      {error, empty_password} ->
          respond_with(422, Req, #{<<"errors">> => #{<<"password">> => <<"can't be blank">>},
                                   <<"status">> => 422,
                                   <<"message">> => <<"validation failed">>});
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

handle(Method, Path, Req) ->
    MethodBin = atom_to_binary(Method, utf8),
    PathBin = binary:list_to_bin(lists:join(<<"/">>, Path)),
    Msg = <<MethodBin/binary, " /", PathBin/binary, " - 404 not found">>,
    respond_with(404, Req, #{<<"message">> => Msg}).

handle_event(_Event, _Data, _Args) ->
    ok.

%% Helpers
handle_publish(Req) ->
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
       server_error ->
            respond_with(500, Req, #{<<"message">> => <<"internal server error">>});
       error ->
           respond_with(401, Req, #{})
   end.

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
        <<"read_only_key">> ->
            unauthorized;
        <<"server_error_key">> ->
            server_error;
        Key when Key =:= <<"key">> orelse Key =:= <<"123">> ->
            {ok, #{username => <<"mr_pockets">>,
                   email => <<"foo@bar.baz">>,
                   organization => <<"hexpm">>,
                   source => key,
                   key => <<"key">>}};
        <<"Bearer ", Token/binary>> when Token =:= <<"test_access_token">> orelse
                                         Token =:= <<"refreshed_access_token">> ->
            {ok, #{username => <<"mr_pockets">>,
                   email => <<"foo@bar.baz">>,
                   organization => <<"hexpm">>,
                   source => oauth,
                   token => Token}};
        <<"unauthorized">> ->
            unauthorized;
        _ ->
            error
    end.
