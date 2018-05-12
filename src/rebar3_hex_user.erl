-module(rebar3_hex_user).

-export([init/1
        ,do/1
        ,format_error/1]).

-export([hex_register/0
        ,whoami/0
        ,auth/0
        ,deauth/0
        ,reset_password/0]).

-include("rebar3_hex.hrl").

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
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex user <command>"},
                                {short_desc, "Hex user tasks"},
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
	    deauth(),
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
    ec_talk:say("By registering an account on Hex.pm you accept all our "
                "policies and terms of service found at https://hex.pm/policies\n"),
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
               "To authenticate again, run `rebar3 hex user auth` "
               "or create a new user with `rebar3 hex user register`", [Username]).

reset_password() ->
    User = ec_talk:ask_default("Username or Email:", string, ""),
    ok = rebar3_hex_http:post_map(filename:join([?ENDPOINT, User, "reset"]), [], maps:new()).

%% Internal functions

get_password() ->
    get_password(<<"Password: ">>).

-define(OP_PUTC, 0).

get_password(confirm) ->
    get_password(<<"Password (confirm): ">>);
get_password(Msg) ->
    case io:setopts([binary, {echo, false}]) of
        ok ->
            PwLine = io:get_line(Msg),
            ok = io:setopts([binary, {echo, true}]),
            io:format("\n"),
            [Pw | _] = binary:split(PwLine, <<"\n">>),
            Pw;
        _ ->
            error_logger:tty(false),
            Port = open_port({spawn, "tty_sl -e"}, [binary, eof]),
            port_command(Port, <<?OP_PUTC, Msg/binary>>),
            receive
                {Port, {data, PwLine}} ->
                    [Pw | _] = binary:split(PwLine, <<"\n">>),
                    port_command(Port, <<?OP_PUTC, $\n>>),
                    port_close(Port),
                    error_logger:tty(true),
                    Pw
            end
    end.

create_user(Username, Email, Password) ->
    case new(Username, Email, Password) of
      {ok, _} ->
        generate_key(Username, Password),
        ec_talk:say("You are required to confirm your email to access your account, "
                    "a confirmation email has been sent to ~s", [Email]);
      {error, StatusCode, Message} ->
        case Message of
          #{<<"message">> := Reason} ->
            ec_talk:say("Registration of user ~s failed (~p, ~s)",
                        [Username, StatusCode, Reason]);
          _ ->
            ec_talk:say("Registration of user ~s failed (~p)",
                        [Username, StatusCode])
        end,
        error
    end.

generate_key(Username, Password) ->
    ec_talk:say("Generating API key..."),
    {ok, Name} = inet:gethostname(),
    case new_key(list_to_binary(Name), Username, Password) of
        {ok, Body} ->
            update_config(Username, Body);
        {error, StatusCode, Body} ->
            ec_talk:say("Generation of API key failed (~p)", [StatusCode]),
            {error, Body}
    end.

new(Username, Email, Password) ->
    rebar3_hex_http:post_map(?ENDPOINT, []
                             ,maps:from_list([{<<"username">>, Username}
                              ,{<<"email">>, Email}
                              ,{<<"password">>, Password}])).

new_key(Name, Username, Password) ->
    Auth = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    rebar3_hex_http:post_map("keys", "Basic "++Auth, maps:from_list([{<<"name">>, Name}])).

update_config(Username, Body)->
    Secret = maps:get(<<"secret">>, Body),
    rebar3_hex_config:update([{username, Username}, {key, Secret}]).
