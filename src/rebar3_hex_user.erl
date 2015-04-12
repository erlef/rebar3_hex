-module(rebar3_hex_user).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([hex_register/0
        ,whoami/0
        ,auth/0
        ,deauth/0
        ,reset_password/0]).

-include("rebar3_hex.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, user).
-define(DEPS, []).

-define(ENDPOINT, "users").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, false},
                                {deps, ?DEPS},
                                {example, "rebar3 hex user <command>"},
                                {short_desc, "."},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["register"] ->
            hex_register(),
            {ok, State};
        ["whoami"] ->
            whoami(),
            {ok, State};
        ["auth"] ->
            auth(),
            {ok, State};
        ["deauth"] ->
            auth(),
            {ok, State};
        ["reset_password"] ->
            reset_password(),
            {ok, State};
        [] ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

hex_register() ->
    Username = list_to_binary(ec_talk:ask_default("Username:", string, "")),
    Email = list_to_binary(ec_talk:ask_default("Email:", string, "")),
    case get_password() of
        <<"">> ->
            error;
        Password ->
            PasswordConfirm = get_password(confirm),
            case Password =:= PasswordConfirm of
                true ->
                    ec_talk:say("Registering..."),
                    create_user(Username, Email, Password);
                false ->
                    error
            end
    end.

whoami() ->
    Username = rebar3_hex_config:username(),
    ec_talk:say(Username).

auth() ->
    Username = list_to_binary(ec_talk:ask_default("Username:", string, "")),
    Password = get_password(),
    generate_key(Username, Password).

deauth() ->
    Username = rebar3_hex_config:username(),
    Config = rebar3_hex_config:read(),
    rebar3_hex_config:write(lists:keydelete(username, 1, lists:keydelete(key, 1, Config))),
    ec_talk:say("User `~s` removed from the local machine. "
               "To authenticate again, run `rebar3 hex.user auth` "
               "or create a new user with `rebar3 hex.user register`", [Username]).

reset_password() ->
    User = ec_talk:ask_default("Username or Email:", string, ""),
    rebar3_hex_http:post_json(filename:join([?ENDPOINT, User, "reset"]), [], []).

%% Internal functions

get_password() ->
    get_password("Password:").

get_password(confirm) ->
    get_password("Password (confirm):");
get_password(Msg) ->
    io:setopts([{echo, false}]),
    Password = list_to_binary(ec_talk:ask_default(Msg, string, "")),
    io:setopts([{echo, true}]),
    io:nl(),
    Password.

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
        {error, Body} ->
            ec_talk:say("Generation of API key failed.", []),
            {error, Body}
    end.

new(Username, Email, Password) ->
    rebar3_hex_http:post_json(?ENDPOINT, []
                             ,[{<<"username">>, Username}
                              ,{<<"email">>, Email}
                              ,{<<"password">>, Password}]).

new_key(Name, Username, Password) ->
    Auth = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    rebar3_hex_http:post_json("keys", "Basic "++Auth, [{<<"name">>, Name}]).

update_config(Username, Body)->
    {<<"secret">>, Secret}  = lists:keyfind(<<"secret">>, 1, Body),
    rebar3_hex_config:update([{username, Username}, {key, Secret}]).
