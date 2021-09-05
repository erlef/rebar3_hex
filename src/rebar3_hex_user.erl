-module(rebar3_hex_user).

-export([init/1,
         do/1,
         format_error/1]).

-export([hex_register/2,
         whoami/2,
         auth/2,
         deauth/2,
         reset_password/2,
         encrypt_write_key/3,
         decrypt_write_key/2,
         decrypt_write_key/3]).

-include("rebar3_hex.hrl").

-define(PROVIDER, user).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex user <command>"},
                                 {short_desc, "Hex user tasks"},
                                 {desc, ""},
                                 {opts, [rebar3_hex:repo_opt()]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["register" | _] ->
            {ok, Repo} = rebar3_hex_config:repo(State),
            hex_register(Repo, State);
        ["whoami" | _] ->
            try handle(whoami, State) of
                _ -> {ok, State}
            catch
                {error, _} = Err ->
                    Err
            end;
        ["auth" | _] ->
            {ok, Repo} = rebar3_hex_config:repo(State),
            auth(Repo, State);
        ["deauth" | _] ->
            {ok, Repo} = rebar3_hex_config:repo(State),
            deauth(Repo, State);
        ["reset_password" | _] ->
            {ok, Repo} = rebar3_hex_config:repo(State),
            reset_password(Repo, State);
        _ ->
            throw(?PRV_ERROR(bad_command))
    end.


-dialyzer({[no_return, no_match], handle/2}).
handle(whoami, State) ->
    {ok, Parents} = rebar3_hex_config:parent_repos(State),
    lists:foreach(fun(R) -> case whoami(R, State) of
                                {ok, _Res} -> ok;
                                {error, _} = Err ->
                                    throw(Err)
                            end
                  end,
                  Parents).


-spec format_error(any()) -> iolist().
format_error({whoami_failure, Reason}) ->
    io_lib:format("Fetching currently authenticated user failed: ~ts", [Reason]);
format_error(bad_local_password) ->
    "Failure to decrypt write key: bad local password";
format_error({registration_failure, Reason}) ->
    io_lib:format("Registration of user failed: ~ts", [Reason]);
format_error({generate_key, Reason}) ->
    io_lib:format("Failure generating authentication tokens: ~ts", [Reason]);
format_error(no_match_local_password) ->
    "Password confirmation failed. The passwords must match.";
format_error(local_password_too_big) ->
    "Local passwords can not exceed 32 characters.";
format_error(bad_command) ->
    "Command must be one of register, whoami, auth, deauth or reset_password";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

hex_register(Repo, State) ->
    rebar3_hex_io:say("By registering an account on Hex.pm you accept all our "
                "policies and terms of service found at https://hex.pm/policies\n"),
    Username = list_to_binary(rebar3_hex_io:ask("Username:", string, "")),
    Email = list_to_binary(rebar3_hex_io:ask("Email:", string, "")),
    case get_account_password() of
        <<"">> ->
            error;
        Password ->
            PasswordConfirm = get_account_password(confirm),
            case Password =:= PasswordConfirm of
                true ->
                    rebar3_hex_io:say("Registering..."),
                    create_user(Username, Email, Password, Repo, State);
                false ->
                    ?PRV_ERROR({error, "passwords do not match"})
            end
    end.

-dialyzer({no_fail_call, whoami/2}).
whoami(#{name := Name} = Repo, State) ->
    case maps:get(read_key, Repo, undefined) of
        undefined ->
            {error, "Not authenticated as any user currently for this repository"};
        ReadKey ->
            case hex_api_user:me(Repo#{api_key => ReadKey}) of
                {ok, {200, _Headers, #{<<"username">> := Username,
                                       <<"email">> := Email}}} ->
                    rebar3_hex_io:say("~ts : ~ts (~ts)", [Name, Username, Email]),
                    {ok, State};
                {ok, {_Status, _Headers, #{<<"message">> := Message}}} ->
                    ?PRV_ERROR({whoami_failure, Message});
                {error, Reason} ->
                    ?PRV_ERROR({whoami_failure, io_lib:format("~p", [Reason])})
            end
    end.

 -dialyzer({no_return, auth/2}).
auth(Repo, State) ->
    Username = list_to_binary(rebar3_hex_io:ask("Username:", string, "")),
    Password = get_account_password(),

    rebar3_hex_io:say("You have authenticated on Hex using your account password. However, "
                "Hex requires you to have a local password that applies only to this machine for security "
                "purposes. Please enter it."),


    MaxRetries = 3,
    LocalPassword = get_local_password(<<"Local Password: ">>, MaxRetries),
    ok = confirm_local_password(<<"Local Password (confirm): ">>, LocalPassword, MaxRetries),
    generate_all_keys(Username, Password, LocalPassword, Repo, State).


get_local_password(_Prompt, 0) -> 
    throw(?PRV_ERROR(local_password_too_big));

get_local_password(Prompt, MaxRetries) ->
    case rebar3_hex_io:get_password(Prompt) of
        Pw when byte_size(Pw) > 32 ->
            Warn = "Local passwords can not be greater than 32 characters, please try again",
            rebar_log:log(warn, Warn, []),
            get_local_password(Prompt, MaxRetries - 1);
        Pw -> 
            Pw
    end.

confirm_local_password(_Prompt, _ExpectedPw, 0) ->
   throw(?PRV_ERROR(no_match_local_password)); 

confirm_local_password(Prompt, ExpectedPw, MaxRetries) -> 
     case rebar3_hex_io:get_password(Prompt) of
        Pw when Pw =:= ExpectedPw ->
            ok;
        _ ->
          Warn = "Passwords do not match, please try again",
          rebar_log:log(warn, Warn, []),
          confirm_local_password(Prompt, ExpectedPw, MaxRetries - 1)
    end.

deauth(#{username := Username, name := RepoName}, State) ->
    rebar3_hex_config:update_auth_config(#{RepoName => #{}}, State),
    rebar3_hex_io:say("User `~s` removed from the local machine. "
                 "To authenticate again, run `rebar3 hex user auth` "
                 "or create a new user with `rebar3 hex user register`", [Username]),
    {ok, State};
deauth(_Repo, State) ->
    rebar3_hex_io:say("Not authenticated as any user currently for this repository"),
    {ok, State}.

reset_password(Repo, State) ->
    User = rebar3_hex_io:ask("Username or Email:", string, ""),
    case hex_api_user:reset_password(Repo, list_to_binary(User)) of
        {ok, {204, _Headers, _Content}} ->
             rebar3_hex_io:say("Email with reset link sent", []),
             {ok, State};
        {ok, {_Status, _Headers, #{<<"message">> := Message}}} ->
            ?PRV_ERROR({reset_failure, Message});
        {error, Reason} ->
            ?PRV_ERROR({reset_failure, io_lib:format("~p", [Reason])})
    end.

%% Internal functions

get_account_password() ->
    rebar3_hex_io:get_password(<<"Account Password: ">>).

get_account_password(confirm) ->
    rebar3_hex_io:get_password(<<"Account Password (confirm): ">>).

-dialyzer({no_fail_call, create_user/5}).
create_user(Username, Email, Password, Repo, State) ->
    case hex_api_user:create(Repo, Username, Password, Email) of
        {ok, {201, _Headers, _Body}} ->
            rebar3_hex_io:say("You are required to confirm your email to access your account, "
                        "a confirmation email has been sent to ~s", [Email]),
            rebar3_hex_io:say("Then run `rebar3 hex auth -r ~ts` to create and configure api tokens locally.",
                        [maps:get(name, Repo)]),
            {ok, State};
        {ok, {_Status, _Headers, #{<<"errors">> := Errors}}} ->
            ?PRV_ERROR({registration_failure,
                        rebar3_hex_client:pretty_print_errors(Errors)});
        {error, Reason} ->
            ?PRV_ERROR({registration_failure, io_lib:format("~p", [Reason])})
    end.

pad(Binary) ->
    case byte_size(Binary) of
        Size when Size =< 16 ->
            <<Binary/binary, 0:((16 - Size) * 8)>>;
        Size when Size =< 24 ->
            <<Binary/binary, 0:((24 - Size) * 8)>>;
        Size when Size =< 32 ->
            <<Binary/binary, 0:((32 - Size) * 8)>>
    end.

-dialyzer({no_return, generate_all_keys/5}).
generate_all_keys(Username, Password, LocalPassword, Repo, State) ->
    rebar3_hex_io:say("Generating all keys..."),

    Auth = base64:encode_to_string(<<Username/binary, ":", Password/binary>>),
    RepoConfig0 = Repo#{api_key => list_to_binary("Basic " ++ Auth)},

    %% write key
    WriteKeyName = api_key_name(),
    WritePermissions = [#{<<"domain">> => <<"api">>}],
    case generate_key(RepoConfig0, WriteKeyName, WritePermissions) of
        {ok, WriteKey} ->

            WriteKeyEncrypted = encrypt_write_key(Username, LocalPassword, WriteKey),

            %% read key
            RepoConfig1 = Repo#{api_key => WriteKey},
            ReadKeyName = api_key_name("read"),
            ReadPermissions = [#{<<"domain">> => <<"api">>, <<"resource">> => <<"read">>}],
            {ok, ReadKey} = generate_key(RepoConfig1, ReadKeyName, ReadPermissions),

            %% repo key
            ReposKeyName = repos_key_name(),
            ReposPermissions = [#{<<"domain">> => <<"repositories">>}],
            {ok, ReposKey} = generate_key(RepoConfig1, ReposKeyName, ReposPermissions),

            % By default a repositories key is created which gives user access to all repositories
            % that they are granted access to server side. For the time being we default
            % to hexpm for user auth entries as there is currently no other use case.
            rebar3_hex_config:update_auth_config(#{?DEFAULT_HEX_REPO => #{
                                                     username => Username,
                                                     write_key => WriteKeyEncrypted,
                                                     read_key => ReadKey,
                                                     repo_key => ReposKey}}, State),
            {ok, State};
        {error, {rebar3_hex_user, _Msg}} = Error ->
            Error
    end.

-ifdef(POST_OTP_22).
-spec encrypt_write_key(binary(), binary(), binary()) -> {binary(), {binary(), binary()}}.
encrypt_write_key(Username, LocalPassword, WriteKey) ->
    AAD = Username,
    IV = crypto:strong_rand_bytes(16),
    Key =  pad(LocalPassword),
    {IV, crypto:crypto_one_time_aead(cipher(Key), Key, IV, WriteKey, AAD, true)}.
-else.
-spec encrypt_write_key(binary(), binary(), binary()) -> {binary(), {binary(), binary()}}.
encrypt_write_key(Username, LocalPassword, WriteKey) ->
    AAD = Username,
    IV = crypto:strong_rand_bytes(16),
    {IV, crypto:block_encrypt(aes_gcm, pad(LocalPassword), IV, {AAD, WriteKey})}.
-endif.

-spec decrypt_write_key(binary(), {binary(), {binary(), binary()}} | undefined) -> binary().
decrypt_write_key(_Username, undefined) ->
    {error, no_write_key};
decrypt_write_key(Username, {IV, {CipherText, CipherTag}}) ->
    LocalPassword = rebar3_hex_io:get_password(<<"Local Password: ">>),
    decrypt_write_key(Username, LocalPassword, {IV, {CipherText, CipherTag}}).

-ifdef(POST_OTP_22).
decrypt_write_key(Username, LocalPassword, {IV, {CipherText, CipherTag}}) ->
    Key = pad(LocalPassword),
    case crypto:crypto_one_time_aead(cipher(Key), Key, IV, CipherText, Username, CipherTag, false) of
        error ->
            throw(?PRV_ERROR(bad_local_password));
        Result ->
            Result
    end.
-else.
decrypt_write_key(Username, LocalPassword, {IV, {CipherText, CipherTag}}) ->
    case crypto:block_decrypt(aes_gcm, pad(LocalPassword), IV, {Username, CipherText, CipherTag}) of
        error ->
            throw(?PRV_ERROR(bad_local_password));
        Result ->
            Result
    end.
-endif.

-ifdef(POST_OTP_22).
cipher(Key) when byte_size(Key) == 16  -> aes_128_gcm;
cipher(Key) when byte_size(Key) == 24  -> aes_192_gcm;
cipher(Key) when byte_size(Key) == 32  -> aes_256_gcm.
-endif.

generate_key(RepoConfig, KeyName, Permissions) ->
    case hex_api_key:add(RepoConfig, KeyName, Permissions) of
        {ok, {201, _Headers, #{<<"secret">> := Secret}}} ->
            {ok, Secret};
        {ok, {_Status, _Headers, #{<<"message">> := Message}}} ->
            ?PRV_ERROR({generate_key, Message});
        {error, Reason} ->
            ?PRV_ERROR({generate_key, io_lib:format("~p", [Reason])})
    end.

hostname() ->
    {ok, Name} = inet:gethostname(),
    Name.

api_key_name() ->
    list_to_binary(hostname()).

api_key_name(Postfix) ->
    list_to_binary([hostname(), "-api-", Postfix]).

repos_key_name() ->
    list_to_binary([hostname(), "-repositories"]).

