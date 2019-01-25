-module(rebar3_hex_integration_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(REPO_CONFIG, maps:merge(hex_core:default_config(), #{
                             name        => <<"foo">>,
                             repo        => <<"foo">>,
                             api_url     => <<"http://127.0.0.1:3000">>,
                             repo_url    => <<"http://127.0.0.1:3000">>,
                             repo_verify => false,
                             read_key                 => <<"123">>,
                             repo_public_key          => <<0>>,
                             repos_key                => <<"repos_key">>,
                             username                 => <<"mr_pockets">>,
                             write_key               => {<<0>>,{<<0>>}}
                            })).

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
    [sanity_check, register_user, register_existing_user, auth_test, whoami_test, deauth_test].

init_per_suite(Config) ->
    meck:new([ec_talk, rebar3_hex_utils], [passthrough, no_link, unstick]),
    Config.

end_per_suite(Config) ->
    meck:unload([ec_talk, rebar3_hex_utils]),
    Config.

init_per_testcase(_Tc, Cfg) ->
    {ok, StorePid} = hex_db:start_link(),
    {ok, MockPid} = elli:start_link([{callback, hex_api_callback}, {port, 3000}]),
    [{hex_store, StorePid}, {hex_mock_server, MockPid} | Cfg].

end_per_testcase(_Tc, Config) ->
    StorePid = ?config(hex_store, Config),
    MockPid = ?config(hex_mock_server, Config),
    ok = hex_db:stop(StorePid),
    ok = elli:stop(MockPid),
    reset_mocks([rebar3_hex_utils, ec_talk]),
    Config.

%%%%%%%%%%%%%%%%%%
%%% Test Cases %%%
%%%%%%%%%%%%%%%%%%

sanity_check(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes">>,
        Email = <<"foo@bar.baz">>,
        Repo = repo_config(),
        {ok, {201, _Headers, Res}} = create_user(User, Pass, Email, Repo),
        ?assertEqual(User, maps:get(<<"username">>, Res)),
        ?assertEqual(Email, maps:get(<<"email">>, Res)),
        ?assert(maps:is_key(<<"inserted_at">>, Res)),
        ?assert(maps:is_key(<<"updated_at">>, Res))
    end.

register_user(_Config) ->
    begin
        Repo = repo_config(),
        setup_mocks_for(register, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        State = mock_command("register", Repo),
        ?assertMatch({ok, State}, rebar3_hex_user:do(State))
    end.

register_existing_user(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes1">>,
        Email = <<"foo@bar.baz">>,
        Repo = repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(register_existing, {<<"mr_pockets1">>, Email, Pass, Repo}),
        ExpReason1 = <<"email already in use">>,
        ExpErr1 = {error, {rebar3_hex_user, {registration_failure, ExpReason1}}},
        State = mock_command("register", Repo),
        ?assertMatch(ExpErr1, rebar3_hex_user:do(State))
    end.

auth_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes1">>,
        Email = <<"foo@bar.baz">>,
        Repo = repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(first_auth, {User, Email, Pass, Repo}),
        AuthState = mock_command("auth", Repo),
        ?assertMatch({ok, AuthState}, rebar3_hex_user:do(AuthState))
    end.

whoami_test(_Config) ->
    begin
        Repo = repo_config(),
        setup_mocks_for(whoami, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        WhoamiState = mock_command("whoami", Repo),
        ?assertMatch({ok, WhoamiState}, rebar3_hex_user:do(WhoamiState))
    end.

deauth_test(_Config) ->
    begin
        Repo = repo_config(),
        setup_mocks_for(deauth, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        DeauthState = mock_command("deauth", Repo),
        ?assertMatch(ok, rebar3_hex_user:do(DeauthState))
    end.

%%%%%%%%%%%%%%%%%%
%%%  Helpers   %%%
%%%%%%%%%%%%%%%%%%

mock_command(Command, Repo) ->
    State0 = rebar_state:new([{command_parsed_args, []}, {resources, []},
                              {hex, [{repos, [Repo]}]}]),
    State1 = rebar_state:add_resource(State0, {pkg, rebar_pkg_resource}),
    State2 = rebar_state:create_resources([{pkg, rebar_pkg_resource}], State1),
    rebar_state:command_args(State2, [Command]).

repo_config() ->
    ?REPO_CONFIG.

create_user(User, Pass, Email, Repo) ->
    hex_api_user:create(Repo, User, Pass, Email).

setup_mocks_for(first_auth, {Username, Email, Password, Repo}) ->
    AuthInfo =  "You have authenticated on Hex using your account password. "
    ++ "However, Hex requires you to have a local password that applies "
    ++ "only to this machine for security purposes. Please enter it.",
    %  #{read_key => <<"secret">>,repos_key => <<"secret">>,
    %   username => <<"mr_pockets">>,
    meck:expect(rebar3_hex_utils, update_auth_config, fun(Cfg, State) ->
                                                              Rname = maps:get(name, Repo),
                                                              Skey = maps:get(repos_key, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{repos_key := Skey, username := Username} = _ = maps:get(Rname, Cfg),
                                                              {ok, State} end),
    meck:expect(rebar3_hex_utils, get_password, fun(_Arg) -> Password end),
    meck:expect(ec_talk, say, fun(Arg) ->
                                      case Arg of
                                          "Generating all keys..." ->
                                              ok;
                                          AuthInfo ->
                                              ok
                                      end
                              end),
    meck:expect(ec_talk, ask_default, fun(Prompt, string, "") ->
                                              case Prompt of
                                                  "Email:" ->
                                                      binary_to_list(Email);
                                                  "Username:" ->
                                                      binary_to_list(Username)
                                              end
                                      end);

setup_mocks_for(whoami, {Username, Email, _Password, _Repo}) ->
    meck:expect(ec_talk, say, fun(Str, Args) ->
                                      case {Str, Args} of
                                          {"~ts (~ts)", [<<Username/binary>>, <<Email/binary>>]} ->
                                              ok
                                      end
                              end);

setup_mocks_for(deauth, {_Username, _Email, _Password, _Repo}) ->
    meck:expect(ec_talk, say, fun(Arg) ->
                                      case Arg of
                                          "Currently not implemented." ->
                                              ok
                                      end
                              end);

setup_mocks_for(register, {Username, Email, Password, Repo}) ->
    ExpectedInfo = "By registering an account on Hex.pm you accept all our"
    ++ " policies and terms of service found at https://hex.pm/policies\n",
    meck:expect(rebar3_hex_utils, update_auth_config, fun(_Cfg, State) -> {ok, State} end),
    meck:expect(rebar3_hex_utils, get_password, fun(_Arg) -> Password end),
    ReqInfo = "You are required to confirm your email to access your account, "
    ++ "a confirmation email has been sent to ~s",
    TokenInfo = "Then run `rebar3 hex auth -r ~ts` to create and configure api tokens locally.",
    RepoName = maps:get(name, Repo),
    meck:expect(ec_talk, say, fun(Str, Args) ->
                                      case {Str, Args} of
                                          {TokenInfo, [RepoName]} ->
                                              ok;
                                          {ReqInfo, [Email]} ->
                                              ok
                                      end
                              end),
    meck:expect(ec_talk, say, fun(Arg) ->
                                      case Arg of
                                          ExpectedInfo ->
                                              ok;
                                          "Registering..." ->
                                              ok
                                      end
                              end),
    meck:expect(ec_talk, ask_default, fun(Prompt, string, "") ->
                                              case Prompt of
                                                  "Email:" ->
                                                      binary_to_list(Email);
                                                  "Username:" ->
                                                      binary_to_list(Username)
                                              end
                                      end);

setup_mocks_for(register_existing, {Username, Email, Password, _Repo}) ->
    ExpectedInfo = "By registering an account on Hex.pm you accept all our"
    ++ " policies and terms of service found at https://hex.pm/policies\n",
    meck:expect(rebar3_hex_utils, get_password, fun(_Arg) -> Password end),
    meck:expect(ec_talk, say, fun(Arg) ->
                                      case Arg of
                                          ExpectedInfo ->
                                              ok;
                                          "Registering..." ->
                                              ok
                                      end
                              end),
    meck:expect(ec_talk, ask_default, fun(Prompt, string, "") ->
                                              case Prompt of
                                                  "Email:" ->
                                                      binary_to_list(Email);
                                                  "Username:" ->
                                                      binary_to_list(Username)
                                              end
                                      end).

reset_mocks(Modules) ->
    meck:reset(Modules),
    [meck:delete(Module, Fun, Arity, false)
     || {Module, Fun, Arity} <- meck:expects(Modules, true)].
