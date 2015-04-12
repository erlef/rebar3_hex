-module(rebar3_hex_user).

-export([register/0
        ,whoami/0
        ,auth/0
        ,deauth/0
        ,reset_password/0]).

-include("rebar3_hex.hrl").

register() ->
    Username = list_to_binary(ec_talk:ask_default("Username: ", string, "")),
    Email = list_to_binary(ec_talk:ask_default("Email: ", string, "")),
    case list_to_binary(ec_talk:ask_default("Password: ", string, "")) of
        <<"">> ->
            error;
        Password ->
            PasswordConfirm = list_to_binary(ec_talk:ask_default("Password (confirm): ", string, "")),
            case Password =:= PasswordConfirm of
                true ->
                    ec_talk:say("Registering..."),
                    create_user(Username, Email, Password);
                false ->
                    error
            end
    end.

whoami() ->
    ok.

auth() ->
    ok.

deauth() ->
    ok.

reset_password() ->
    ok.

%% Internal functions

create_user(Username, Email, Password) ->
    case new(Username, Email, Password) of
      {ok, _} ->
        generate_key(Username, Password),
        ec_talk:say("You are required to confirm your email to access your account, "
                    "a confirmation email has been sent to ~s", [Email]);
      _ ->
        ec_talk:say("Registration of user ~s failed", [Username]),
        error
    end.


generate_key(Username, Password) ->
    ec_talk:say("Generating API key..."),
    {ok, Name} = inet:gethostname(),
    case new_key(list_to_binary(Name), Username, Password) of
        {ok, Body} ->
            update_config(Username, Body);
        {Code, Body} ->
            ec_talk:say("Generation of API key failed (~p)", [Code]),
            {error, Code, Body}
    end.

new(Username, Email, Password) ->
    rebar3_hex_http:post_json("users", []
                             ,[{<<"username">>, Username}
                              ,{<<"email">>, Email}
                              ,{<<"password">>, Password}]).

new_key(Name, Username, Password) ->
    Auth = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    rebar3_hex_http:post_json("keys", "Basic "++Auth, [{<<"name">>, Name}]).

update_config(Username, Body)->
    {<<"secret">>, Secret}  = lists:keyfind(<<"secret">>, 1, Body),
    rebar3_hex_config:update([{username, Username}, {key, Secret}]).
