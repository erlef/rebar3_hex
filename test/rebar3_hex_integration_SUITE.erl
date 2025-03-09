-module(rebar3_hex_integration_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
    [
     sanity_check
    , decrypt_write_key_test
    , bad_command_test
    , reset_password_test
    , reset_local_password_test
    , reset_password_error_test
    , reset_password_api_error_test
    , register_user_test
    , register_empty_password_test
    , register_password_mismatch_test
    , register_error_test
    , register_existing_user_test
    , auth_test
    , auth_bad_local_password_test
    , auth_password_24_char_test
    , auth_password_32_char_test
    , auth_unhandled_test
    , auth_error_test
    , whoami_test
    , whoami_not_authed_test
    , whoami_error_test
    , whoami_unknown_test
    , deauth_test
    , org_auth_test
    , org_auth_key_test
    , org_deauth_test
    , org_key_generate_test
    , org_key_revoke_test
    , org_key_revoke_all_test
    , org_key_list_test
    , org_list_test
    , org_bad_command_test
    , build_package_test
    , build_docs_test
    , build_test
    , publish_test
    , publish_no_prompt_test
    , publish_package_with_pointless_app_arg_test
    , publish_with_checkouts_warnings_test
    , publish_abort_test
    , publish_in_umbrella_test
    , publish_package_test
    , publish_multi_errors_test
    , publish_in_umbrella_when_app_does_not_exist
    , publish_package_in_umbrella_with_app_opt_test
    , publish_docs_in_umbrella_with_app_opt_test
    , publish_package_in_umbrella_without_app_opt_test
    , publish_docs_in_umbrella_without_app_opt_test
    , publish_package_in_umbrella_when_does_not_exist_test
    , publish_docs_in_umbrella_when_does_not_exist_test
    , publish_replace_test
    , publish_revert_test
    , publish_revert_in_umbrella_test
    , publish_revert_in_umbrella_without_app_arg_test
    , publish_revert_in_umbrella_app_not_found_test
    , publish_revert_package_test
    , publish_revert_invalid_version_test
    , publish_revert_docs_test
    , publish_org_test
    , publish_org_non_hexpm_test
    , publish_org_error_test
    , publish_org_requires_repo_arg_test
    , publish_error_test
    , publish_unauthorized_test
    , publish_docs_test
    , publish_docs_dry_run_test
    , retire_test
    , key_list_test
    , key_get_test
    , key_add_test
    , key_delete_test
    , key_delete_all_test
    , owner_add_test
    , owner_transfer_test
    , owner_list_test
    , owner_remove_test
    ].

init_per_suite(Config) ->
    meck:new([hex_api_user, rebar3_hex_config, rebar3_hex_io], [passthrough, no_link, unstick]),
    Config.

end_per_suite(Config) ->
    meck:unload([rebar3_hex_config, rebar3_hex_io, hex_api_user]),
    Config.

init_per_testcase(_Tc, Cfg) ->
    {ok, StorePid} = hex_db:start_link(),
    {ok, MockPid} = elli:start_link([{callback, hex_api_model}, {port, 3000}]),
    [{hex_store, StorePid}, {hex_mock_server, MockPid} | Cfg].

end_per_testcase(_Tc, Config) ->
    StorePid = ?config(hex_store, Config),
    MockPid = ?config(hex_mock_server, Config),
    ok = hex_db:stop(StorePid),
    ok = elli:stop(MockPid),
    reset_mocks([rebar3_hex_config, rebar3_hex_io, hex_api_user]),
    Config.

data_dir(Config) -> ?config(data_dir, Config).

%%%%%%%%%%%%%%%%%%
%%% Test Cases %%%
%%%%%%%%%%%%%%%%%%

-define(default_username, <<"mr_pockets">>).
-define(default_password, <<"special_shoes">>).
-define(default_email,  <<"foo@bar.baz">>).
-define(default_key_phase, <<"key">>).

-define(default_params, #{username => ?default_username,
                          password => ?default_password,
                          password_confirmation => ?default_password,
                          email => ?default_email,
                          key_phrase => ?default_key_phase
                         }).

-define(default_repo, <<"hexpm">>).

-define(default_repo_config, test_utils:default_config()).

sanity_check(_Config) ->
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        {ok, {201, _Headers, Res}} = create_user(User, Pass, Email, Repo),
        ?assertEqual(User, maps:get(<<"username">>, Res)),
        ?assertEqual(Email, maps:get(<<"email">>, Res)),
        ?assert(maps:is_key(<<"inserted_at">>, Res)),
        ?assert(maps:is_key(<<"updated_at">>, Res)).

%% We test this specific function here as it requires a mock.
decrypt_write_key_test(_Config) ->
    Repo = test_utils:repo_config(),
    WriteKey = ?default_key_phase,
    Username = ?default_username,
    LocalPassword = ?default_password,
    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    % This combo is one byte off in the IV
    BadKey = {<<21,112,99,26,160,67,34,190,25,135,85,235,132,185,94,88>>,
              {<<230,231,88,0>>,<<216,113,132,52,197,237,230,15,200,76,130,195,236,21,203,77>>}},

    setup_mocks_for(decrypt_write_key, #{username => Username, email => ?default_email, password =>
                                         LocalPassword, password_confirmation => LocalPassword, repo => Repo}),

    ?assertError({error,{rebar3_hex_user,bad_local_password}}, rebar3_hex_user:decrypt_write_key(<<"mr_pockets">>,
                                                                                                 BadKey)),
    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, WriteKeyEncrypted)).

register_user_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["register"]},
          app => #{name => "valid"},
          mocks => [register],
          username => <<"jlundegaard">>,
          email => <<"jlundegaard@gustafson-mortors.com">>
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

register_existing_user_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["register"]},
          app => #{name => "valid"},
          mocks => [register],
          username => <<"not_taken">>,
          email => ?default_email
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ExpErr1 = {error, {rebar3_hex_user, {registration_failure,#{<<"email">> => <<"already in use">>}}}},
    ?assertError(ExpErr1, rebar3_hex_user:do(State)).

register_empty_password_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["register"]},
          app => #{name => "valid"},
          mocks => [register],
          password => <<>>,
          password_confirmation => <<>>
         },
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error, {rebar3_hex_user, {registration_failure,#{<<"password">> => <<"can't be blank">>}}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)).

register_error_test(Config) ->
    meck:expect(hex_api_user, create, fun(_,_,_,_) -> {error, meh} end),
    P = #{
          command => #{provider => rebar3_hex_user, args => ["register"]},
          app => #{name => "valid"},
          mocks => [register]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error,{rebar3_hex_user,{registration_failure,{error, meh}}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)).

register_password_mismatch_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["register"]},
          app => #{name => "valid"},
          mocks => [register],
          password_confirmation => <<"special_shoes0">>
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ExpErr = {error,{rebar3_hex_user,passwords_do_not_match}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)).

auth_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["auth"]},
          app => #{name => "valid"},
          mocks => [first_auth]
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

org_auth_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["auth", "hexpm:foo"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => []
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expect_local_password_prompt(Setup),
    expects_repo_config(Setup),
    expects_update_auth_config(Setup),
    expects_update_auth_config_for(<<"hexpm:foo">>),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_auth_key_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["auth", "hexpm:foo", "--key", "123"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => []
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expect_local_password_prompt(Setup),
    expects_repo_config(Setup),
    expects_update_auth_config(Setup),
    expects_update_auth_config_for(<<"hexpm:foo">>),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_deauth_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["deauth", "hexpm:foo"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => []
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_output([{"Successfully deauthorized ~ts", [<<"hexpm:foo">>]}]),
    expects_repo_config(Setup),

    expects_update_auth_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_key_generate_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["key", "hexpm:foo", "generate"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => [first_auth]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    expect_local_password_prompt(Setup),
    expects_update_auth_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_key_revoke_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["key", "hexpm:foo", "revoke", "--key-name", "this-key"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => [key_mutation]
         },
    #{rebar_state := State, repo := _Repo} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    expect_local_password_prompt(Setup),
    expects_update_auth_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_key_revoke_all_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["key", "hexpm:foo", "revoke", "--all"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => [key_mutation]
         },
    #{rebar_state := State, repo := _Repo} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    expects_update_auth_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_key_list_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["key", "hexpm:foo", "list"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => [first_auth]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    expects_update_auth_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_list_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["list"]},
          app => #{name => "valid"},
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>},
          mocks => [first_auth]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_organization:do(State)).

org_bad_command_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_organization, args => ["key", "hexpm:foo", "eh", "--all"]},
          app => #{name => "valid"},
          mocks => [key_mutation]
         },
    #{rebar_state := State, repo := _Repo} = _Setup = setup_state(P, Config),
    ?assertError({error, {rebar3_hex_organization,bad_command}}, rebar3_hex_organization:do(State)).

auth_bad_local_password_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["auth"]},
          app => #{name => "valid"},
          mocks => [first_auth],
          password_confirmation => <<"oops">>
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertError({error,{rebar3_hex_user,passwords_do_not_match}}, rebar3_hex_user:do(State)).

auth_password_24_char_test(Config) ->
    Pass = <<"special_shoes_shoes">>,
    P = #{
          command => #{provider => rebar3_hex_user, args => ["auth"]},
          app => #{name => "valid"},
          mocks => [first_auth],
          password => Pass,
          password_confirmation => Pass
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

auth_password_32_char_test(Config) ->
    Pass = <<"special_shoes_shoes_shoes">>,
    P = #{
          command => #{provider => rebar3_hex_user, args => ["auth"]},
          app => #{name => "valid"},
          mocks => [first_auth],
          password => Pass,
          password_confirmation => Pass
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

auth_unhandled_test(Config) ->
    meck:new([hex_api_key]),
    MeckReturn = {ok, {500, #{}, #{<<"message">> => <<"eh?">>}}},
    meck:expect(hex_api_key, add, fun(_,_,_) -> MeckReturn end),
    P = #{command => #{provider => rebar3_hex_user, args => ["auth"]}, app => #{name => "valid"}, mocks => [first_auth]},
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error,{rebar3_hex_user,{generate_key,<<"eh?">>}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)),
    meck:unload([hex_api_key]).

auth_error_test(Config) ->
    %% TODO: Revise hex_api_model and hex_db so that we don't need to meck this
    meck:new([hex_api_key]),
    meck:expect(hex_api_key, add, fun(_,_,_) -> {error, meh} end),

    P = #{
          command => #{provider => rebar3_hex_user, args => ["auth"]},
          app => #{name => "valid"},
          mocks => [first_auth]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error,{rebar3_hex_user,{generate_key,{error, meh}}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)),
    meck:unload([hex_api_key]).

whoami_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["whoami"]},
          app => #{name => "valid"},
          mocks => [whoami]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

whoami_unknown_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["whoami"]},
          app => #{name => "valid"},
          mocks => [whoami],
          repo_config => #{read_key => <<"eh?">>}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_parent_repos(Setup),
    ExpErr = {error,{rebar3_hex_user,{whoami,<<"huh?">>}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)).

whoami_api_error_test(Config) ->
    meck:expect(hex_api_user, me, fun(_) -> {error, meh} end),
    P = #{
          command => #{provider => rebar3_hex_user, args => ["whoami"]},
          app => #{name => "valid"},
          mocks => [whoami],
          repo_config =>
          #{read_key => <<"!">>}
         },
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error,{rebar3_hex_user,{whoami_failure,["meh"]}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(State)).

whoami_error_test(Config) ->
    meck:expect(hex_api_user, me, fun(_) -> {error, meh} end),
    P = #{
          command => #{provider => rebar3_hex_user, args => ["whoami"]},
          app => #{name => "valid"}, mocks => [whoami]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error,{rebar3_hex_user,{whoami,{error, meh}}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)).

whoami_not_authed_test(Config) ->
    meck:expect(hex_api_user, me, fun(_) -> {error, meh} end),
    P = #{
          command => #{provider => rebar3_hex_user, args => ["whoami"]},
          app => #{name => "valid"},
          mocks => [whoami],
          repo_config => #{read_key => undefined}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_parent_repos(Setup),
    ?assertError({error, {rebar3_hex_user,not_authenticated}}, rebar3_hex_user:do(State)).

reset_password_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["reset_password", "account"]},
          app => #{name => "valid"},
          mocks => [reset_password],
          username => "mr_pockets"
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

reset_local_password_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["reset_password", "local"]},
          app => #{name => "valid"},
          mocks => [key_mutation],
          username => "mr_pockets"
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_update_auth_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

reset_password_api_error_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["reset_password", "account"]},
          app => #{name => "valid"},
          mocks => [reset_password],
          username => "eh?",
          repo_config => #{username => "eh?"}
         },
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error,{rebar3_hex_user,{reset_account_password,<<"huh?">>}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)).

reset_password_error_test(Config) ->
    meck:expect(hex_api_user, reset_password, fun(_,_) -> {error, meh} end),
    P = #{
          command => #{provider => rebar3_hex_user, args => ["reset_password", "account"]},
          app => #{name => "valid"},
          mocks => [reset_password],
          username => "mr_pockets"
         },
    #{rebar_state := State} = setup_state(P, Config),
    ExpErr = {error,{rebar3_hex_user,{reset_account_password,{error, meh}}}},
    ?assertError(ExpErr, rebar3_hex_user:do(State)).

deauth_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["deauth"]},
          app => #{name => "valid"},
          mocks => [deauth]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

build_package_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_build, args => ["package"]},
          app => #{name => "valid"},
          mocks => []
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, _}, rebar3_hex_build:do(State)).

build_docs_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_build, args => ["docs"]},
          app => #{name => "valid_docs"},
          mocks => []
         },
    #{rebar_state := State, repo := _Repo} = Setup = setup_state(P, Config),
    %State1 = rebar_state:set(State, hex, [{repos, [Repo]}, {doc, edoc}]),
    expects_repo_config(Setup),
    ?assertMatch({ok, _}, rebar3_hex_build:do(State)).


build_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_build, args => []},
          app => #{name => "valid"},
          mocks => []
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, _}, rebar3_hex_build:do(State)).

publish_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => []},
          app => #{name => "valid"},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_no_prompt_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--yes"]},
          app => #{name => "valid"},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_package_with_pointless_app_arg_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["package", "--app", "valid"]},
          app => #{name => "valid"},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_with_checkouts_warnings_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => []},
          app => #{name => "valid"},
          has_checkouts => true,
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_abort_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => []},
          app => #{name => "valid"},
          mocks => [publish_abort]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_in_umbrella_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--app", "foo"]},
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_in_umbrella_when_app_does_not_exist(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--app", "eh"]},
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    Exp = {error,{rebar3_hex_publish,{app_not_found,"eh"}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_package_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["package"]},
          app => #{name => "valid"}, mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_docs_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["docs"]},
          app => #{name => "valid"},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_package_in_umbrella_with_app_opt_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["package", "--app", "foo"]},
          type => "umbrella",
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_docs_in_umbrella_with_app_opt_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["docs", "--app", "foo"]},
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_package_in_umbrella_without_app_opt_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["package"]},
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    Exp = {error, {rebar3_hex_publish, {publish_package, app_switch_required}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_package_in_umbrella_when_does_not_exist_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["package", "--app", "eh"]},
          type => "umbrella",
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    Exp = {error,{rebar3_hex_publish,{app_not_found,"eh"}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_docs_in_umbrella_when_does_not_exist_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["docs", "--app", "eh"]},
          type => "umbrella",
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    Exp = {error,{rebar3_hex_publish,{app_not_found,"eh"}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_docs_in_umbrella_without_app_opt_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["docs"]},
          type => "umbrella",
          umbrella => #{
                        name => "umbrella",
                        apps => [#{name => "foo", selected => true}, #{name => "bar"}, #{name => "baz"}]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    Exp = {error, {rebar3_hex_publish, {publish_docs, app_switch_required}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).


publish_multi_errors_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => []},
          app => #{name => "foo", app_src => #{version => "0.1b-prod"}},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    Exp = {error, {rebar3_hex_publish,
                         {validation_errors,
                          [{invalid_semver,
                            {<<"foo">>,"0.1b-prod"}}]}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).


% %% TODO: This test currently is merely to see if we can handle the --replace switch
% %% In order for the test to be more meaningful we need to update the hex_api_model to keep
% %% track of packages that have been published and when, further we need to provide
% %% a package add function on hex_api_model which takes a package name, published at timestamp, etc.
% %% so we can test the sad paths (i.e., you can not replace a package after N seconds)
publish_replace_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--replace"]},
          app => #{name => "valid"},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_revert_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--revert", "1.0.0"]},
          app => #{name => "valid", version => "1.0.0", app_src => #{version => "1.0.0"}},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_revert_in_umbrella_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--revert", "1.0.0", "--app", "foo"]},
          umbrella => #{
                        name => "umbrella",
                        apps => [
                                 #{name => "foo", version => "1.0.0", selected => true},
                                 #{name => "bar"},
                                 #{name => "baz"}
                                ]
                       },
          mocks => [publish],
          version => "1.0.0"
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_revert_in_umbrella_without_app_arg_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--revert", "1.0.0"]},
          umbrella => #{
                        name => "umbrella",
                        apps => [
                                 #{name => "foo", selected => true, version => "1.0.0"},
                                 #{name => "bar"}, #{name => "baz"}
                                ]
                       },
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    Exp = {error,{rebar3_hex_publish,{revert, app_switch_required}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_revert_in_umbrella_app_not_found_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--revert", "1.0.0", "--app", "no_baz"]},
          umbrella => #{name => "umbrella", apps => [#{name => "foo", version => "1.0.0"}, #{name => "bar"}]},
          mocks => [publish]
         },
    #{rebar_state := State} = setup_state(P, Config),
    Exp = {error,{rebar3_hex_publish,{app_not_found,"no_baz"}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_revert_package_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["package", "--revert", "1.0.0"]},
          app => #{name => "valid", version => "1.0.0", app_src => #{version => "1.0.0"}},
          mocks => [publish],
          version => "1.0.0"
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_revert_invalid_version_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["--revert", "1.0"]},
          app => #{name => "valid", app_src => #{version => "1.0.0"}},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    Exp = {error, {rebar3_hex_publish, {invalid_semver_arg,<<"1.0">>}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_revert_docs_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["docs", "--revert", "1.0.0"]},
          app => #{name => "valid", app_src => #{version => "1.0.0"}},
          mocks => [publish],
          version => "1.0.0"
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_org_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["-r", "hexpm:valid"]},
          app => #{name => "valid"},
          mocks => [publish],
          repo_config => #{repo => <<"hexpm:valid">>, name => <<"hexpm:valid">>}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_org_non_hexpm_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["-r", "other"]},
          app => #{name => "valid"},
          mocks => [publish],
          repo_config => #{repo => <<"other">>,name => <<"other">>}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

publish_org_error_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["-r", "hexpm:bar"]},
          app => #{name => "valid"},
          mocks => [],
          repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_update_auth_config(Setup),
    expect_local_password_prompt(Setup),
    Exps = [
            {"Proceed?", boolean, "Y", {returns, true}},
            {any, string, "A", {return, "A"}}
           ],

    expects_prompts(Exps),
    expects_output(default_publish_io(Setup)),

    ExpError = {error,{rebar3_hex_publish,{not_valid_repo,"hexpm:bar"}}},
    ?assertError(ExpError, rebar3_hex_publish:do(State)).

publish_org_requires_repo_arg_test(Config) ->
    P = #{command => #{provider => rebar3_hex_publish, args => []},
          app  => #{name => "valid"},
          mocks => [],
          repo_config => #{repo => <<"hexpm:valid">>, name => <<"hexpm:valid">>}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_update_auth_config(Setup),
    expect_local_password_prompt(Setup),
    Exps = [
            {"Proceed?", boolean, "Y", {returns, true}},
            {any, string, "A", {return, "A"}}
           ],

    expects_prompts(Exps),
    expects_output(default_publish_io(Setup)),
    ?assertError({error,{rebar3_hex_publish,{required,repo}}}, rebar3_hex_publish:do(State)).

publish_error_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => []},
          app => #{name => "valid"},
          mocks => [publish],
          repo_config => #{write_key => undefined}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertError({error,{rebar3_hex_publish,{get_hex_config, no_write_key}}}, rebar3_hex_publish:do(State)).

publish_unauthorized_test(Config) ->
    WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"unauthorized">>),
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["-r", "hexpm:valid"]},
          app => #{name => "valid"},
          mocks => [publish],
          repo_config => #{write_key => WriteKey, repo => <<"hexpm:valid">>, name => <<"hexpm:valid">>}
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    Exp = {error,
           {rebar3_hex_publish,
            {publish_package,
             <<"valid">>,
             "0.1.0",
             {error,
              #{<<"message">> =>
                <<"account not authorized for this action">>}}}}},
    ?assertError(Exp, rebar3_hex_publish:do(State)).

publish_docs_dry_run_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_publish, args => ["docs", "--dry-run"]},
          app => #{name => "valid"},
          mocks => [publish]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_repo_config(Setup),
    ?assertMatch({ok, State}, rebar3_hex_publish:do(State)).

retire_test(Config) ->
    Command1 = #{provider => rebar3_hex_retire, args => []},
    P1 = #{command => Command1, app => #{name => "valid"}, mocks => [retire]},
    #{rebar_state := State1} = setup_state(P1, Config),
    ?assertError({error,{rebar3_hex_retire,bad_command}}, rebar3_hex_retire:do(State1)),

    Command2 = Command1#{args => ["pkg_name"]},
    P2 = #{command => Command2, app => #{name => "valid"}, mocks => [retire]},
    #{rebar_state := State2} = setup_state(P2, Config),
    ?assertError({error,{rebar3_hex_retire,bad_command}}, rebar3_hex_retire:do(State2)),

    Command3 = Command1#{args => ["pkg_name", "1.1.1"]},
    P3 = #{command => Command3, app => #{name => "valid"}, mocks => [retire]},
    #{rebar_state := State3} = setup_state(P3, Config),
    ?assertError({error,{rebar3_hex_retire,bad_command}}, rebar3_hex_retire:do(State3)),

    Command4 = Command1#{args => ["pkg_name", "1.1.1", "reason"]},
    P4 = #{command => Command4, app => #{name => "valid"}, mocks => [retire]},
    #{rebar_state := State4} = setup_state(P4, Config),
    ?assertError({error,{rebar3_hex_retire,{required,message}}}, rebar3_hex_retire:do(State4)),

    Command5 = Command1#{args => ["pkg_name", "1.1.1", "reason", "--message", "msg"]},
    P5 = #{command => Command5, app => #{name => "valid"}, mocks => [retire]},
    #{rebar_state := State5} = setup_state(P5, Config),
    ?assertMatch({ok, State5}, rebar3_hex_retire:do(State5)).

key_list_test(Config) ->
    P = #{command => #{provider => rebar3_hex_user, args => ["key", "list"]}, app => #{name => "valid"}, mocks => []},
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

key_get_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["key", "fetch", "-k", "key"]},
          app => #{name => "valid"}, mocks => []
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

key_add_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["key", "generate", "-k", "foo"]},
          app => #{name => "valid"},
          mocks => [key_mutation]
         },
    #{rebar_state := State} = Setup = setup_state(P, Config),
    expects_user_registration_prompts(Setup),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

key_delete_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["key", "revoke", "-k", "key"]},
          app => #{name => "valid"},
          mocks => [key_mutation]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

key_delete_all_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_user, args => ["key", "revoke", "--all"]},
          app => #{name => "valid"},
          mocks => [key_mutation]
         },
    #{rebar_state := State} = setup_state(P, Config),
    ?assertMatch({ok, State}, rebar3_hex_user:do(State)).

owner_add_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_owner, args =>  ["add", "truecoat", "wade@foo.bar"]},
          app => #{name => "valid"},
          mocks => [owner]
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertMatch({ok, State}, rebar3_hex_owner:do(State)).

owner_transfer_test(Config) ->
    P = #{
          command => #{
                       provider => rebar3_hex_owner,
                       args => ["transfer", "truecoat", "gustafson_motors", "-r", "hexpm"]
                      },
          app => #{name => "valid"},
          mocks => [owner]
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertMatch({ok, State}, rebar3_hex_owner:do(State)).

owner_remove_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_owner, args => ["remove", "truecoat", "wade@foo.bar"]},
          app => #{name => "valid"},
          mocks => [owner]
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertMatch({ok, State}, rebar3_hex_owner:do(State)).

owner_list_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_owner, args => ["list", "truecoat"]},
          app => #{name => "valid"},
          mocks => [owner]
         },
    #{rebar_state := State, repo := Repo} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ?assertMatch({ok, State}, rebar3_hex_owner:do(State)).

bad_command_test(Config) ->
    P = #{command => #{provider => rebar3_hex_user, args => ["bad_command"]}, app => #{name => "valid"},
                                                                                           mocks => [deauth]},
    #{rebar_state := State} = setup_state(P, Config),
    ?assertError({error,{rebar3_hex_user,bad_command}}, rebar3_hex_user:do(State)).

revert_invalid_ver_test(Config) ->
    P = #{
          command => #{provider => rebar3_hex_revert, args => ["valid", "eh?"]},
          app => #{name => "valid"},
          mocks => [publish_revert],
          version => "eh?"
         },
    #{rebar_state := State} = setup_state(P, Config),
     ?assertThrow(rebar_abort, rebar3_hex_revert:do(State)).

%%%%%%%%%%%%%%%%%%
%%%  Helpers   %%%
%%%%%%%%%%%%%%%%%%

%% setup_state/2 takes a set of parameters and a CT config
%% The with param is used to setup predefined mock scenarios. May be an empty list if no mocks are required.
setup_state(P, Config) ->
    Params = maps:merge(?default_params, P),
    #{mocks := Mocks,
      username := Username,
      password := Password,
      password_confirmation := _PasswordConfirm,
      key_phrase := KeyPhrase,
      email := _Email}            = Params,

    WriteKey = maps:get(write_key, Params, rebar3_hex_user:encrypt_write_key(Username, Password, KeyPhrase)),
    DefRepoConfig = test_utils:default_config(),
    ParamRepoConfig =  maps:get(repo_config, Params, DefRepoConfig),
    RepoConfig = maps:merge(DefRepoConfig#{write_key => WriteKey}, ParamRepoConfig),
    Repo = test_utils:repo_config(RepoConfig),

    StubSpec = get_stub_spec(P),
    DataDir =  filename:join([?config(data_dir, Config), "test_apps"]),
    {State, _Repo, App} = test_utils:make_stub(StubSpec#{repo => Repo, dir => DataDir}),

%    App = case rebar_state:project_apps(State) of
%         [App1] ->
%             App1;
%         [_|_] = Apps ->
%           #{app := ProjAppName} = P,
%           {ok, App1} = rebar3_hex_app:find(Apps, ProjAppName),
%           App1
%     end,

    %% Make sure there is no leftover generated doc or _checkouts directory
    CheckoutsDir = filename:join(rebar_app_info:dir(App), "_checkouts"),

    #{name := ProjectName} = StubSpec,
    case P of
        #{has_checkouts := true} ->
            file:make_dir(CheckoutsDir);
        _ ->
          ok = rebar_file_utils:rm_rf(CheckoutsDir)
    end,

    ok = rebar_file_utils:rm_rf(filename:join(rebar_app_info:dir(App), "doc")),


    Selected = case P of
                #{umbrella := #{apps := Apps1}} ->
                    case lists:filter(fun(App1) -> maps:is_key(selected, App1) end, Apps1) of
                        [Found] ->
                            Found;
                        _ ->
                            none
                    end;
                _ ->
                      maps:get(app, P, ProjectName)

              end,

    Setup = Params#{ repo => Repo,
                     selected_app => Selected,
                     rebar_state => State
                   },

    lists:foreach(fun(W) -> setup_mocks_for(W, Setup) end, Mocks),

    #{command := #{provider := Provider, args := Args}} = Setup,
    {ok, State1} = test_utils:mock_command(Provider, Args, [{repos, [Repo]}, {doc, edoc}], State),

    Setup#{rebar_state => State1}.

get_stub_spec(#{lib := StubSpec}) -> StubSpec#{type => lib};
get_stub_spec(#{app := StubSpec}) -> StubSpec#{type => app};
get_stub_spec(#{umbrella := StubSpec}) -> StubSpec#{type => umbrella}.

create_user(User, Pass, Email, Repo) ->
    hex_api_user:create(Repo, User, Pass, Email).

setup_mocks_for(decrypt_write_key, Setup) ->
    expect_local_password_prompt(Setup);

setup_mocks_for(org_auth, #{repo := Repo}) ->
    meck:expect(rebar3_hex_config, update_auth_config, fun(Cfg, _State) ->
                                                              Rname = maps:get(name, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{auth_key := <<"key">>} = _ = maps:get(Rname, Cfg),
                                                              ok end);

setup_mocks_for(docs, #{repo := Repo} = Setup) ->
    meck:expect(rebar3_hex_config, update_auth_config, fun(Cfg, State) ->
                                                              Rname = maps:get(name, Repo),
                                                              Skey = maps:get(repo_key, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{repo_key := Skey} = _ = maps:get(Rname, Cfg),
                                                              {ok, State} end),

    expect_local_password_prompt(Setup),
	expects_prompts([{any, string, "A", {returns, "A"}}, {"Proceed?", boolean, "Y", {returns, true}}]),
    expects_output(default_publish_io(Setup));

setup_mocks_for(first_auth, #{username := Username,
                              repo := Repo} = Setup) ->

    AuthInfo =  "You have authenticated on Hex using your account password. "
    ++ "However, Hex requires you to have a local password that applies "
    ++ "only to this machine for security purposes. Please enter it.",

    Fun = fun(Cfg, State) ->
                  Rname = maps:get(repo_name, Repo),
                  Skey = maps:get(repo_key, Repo),
                  [Rname] = maps:keys(Cfg),
                  #{repo_key := Skey, username := Username} = maps:get(Rname, Cfg),
                  {ok, State} end,

    meck:expect(rebar3_hex_config, update_auth_config, Fun),
    expect_local_password_prompt(Setup),
    expects_output(["Generating keys...", AuthInfo, "You are now ready to interact with your hex repositories."]),
    expects_user_registration_prompts(Setup);

setup_mocks_for(owner, #{password := Password} = Setup) ->
    expects_repo_config(Setup),
    meck:expect(rebar3_hex_io, get_password, fun(_Arg) -> Password end),
    Fun = fun(Templ, Args) ->
                  case {Templ, Args} of
                      {_, [_UserOrOrg, _Package]} -> %% add/remove
                          ok;
                      {"~s",["unspecified (unspecified)"]} ->  %% list
                          ok
                  end
          end,
    meck:expect(rebar3_hex_io, say, Fun);

setup_mocks_for(whoami, #{username := Username,
                          email := Email,
                          repo := #{name := Name}} = Setup) ->

    Args =  [<<Name/binary>>, <<Username/binary>>, <<Email/binary>>],
    Expects = [{"~ts : ~ts (~ts)", Args}],
    expects_repo_config(Setup),
    expects_output(Expects);

setup_mocks_for(reset_password, #{username := Username}) ->
    expects_prompts([{any, string, "", {returns, Username}}]),
    expects_output([{"Email with reset link sent", []}]);

setup_mocks_for(deauth, #{username := Username, repo := #{name := RepoName}}) ->
    Fun = fun(#{RepoName := #{}}, State) -> {ok, State} end,
    meck:expect(rebar3_hex_config, update_auth_config, Fun),
    Templ1 = "User `~s` removed from the local machine. To authenticate again, run `rebar3 hex ",
    Templ2 = "user auth` or create a new user with `rebar3 hex user register`",
    ExpTempl = string:concat(Templ1, Templ2),
    expects_output([{ExpTempl, [Username]}]);

setup_mocks_for(key_mutation, Setup) ->
    expects_repo_config(Setup),
    expect_local_password_prompt(Setup);

setup_mocks_for(publish, #{selected_app := none} = Setup) ->
    expects_update_auth_config(Setup),
    expects_repo_config(Setup),
    expect_local_password_prompt(Setup),
    Proceed = case Setup of
                  #{has_checkouts := true} ->
                      {"Proceed (with warnings)?", boolean, "Y", {returns, true}};
                 _ ->
                      {"Proceed?", boolean, "Y", {returns, true}}
              end,

    Exps = [
            {any, string, "A", {return, "A"}},
            Proceed
           ],
    expects_prompts(Exps);

setup_mocks_for(publish, Setup) ->
    expects_update_auth_config(Setup),
    expects_repo_config(Setup),
    expect_local_password_prompt(Setup),
    Proceed = case Setup of
                  #{has_checkouts := true} ->
                      {"Proceed (with warnings)?", boolean, "Y", {returns, true}};
                 _ ->
                      {"Proceed?", boolean, "Y", {returns, true}}
              end,

    Exps = [
            {any, string, "A", {return, "A"}},
            Proceed
           ],

    Exps1 = case Setup of
        #{selected_app := #{version := Vsn}} ->
           VsnExp = {io_lib:format("Also delete tag v~s?", [Vsn]),boolean,"N", {returns, false}},
           [VsnExp | Exps];
         _ ->
           Exps
        end,

    expects_prompts(Exps1),
    expects_output(default_publish_io(Setup));

setup_mocks_for(publish_abort, #{username := Username, repo := Repo} = Setup) ->
    Fun = fun(Cfg, State) ->
                  Rname = maps:get(name, Repo),
                  Skey = maps:get(repo_key, Repo),
                  [Rname] = maps:keys(Cfg),
                  #{repo_key := Skey, username := Username} = maps:get(Rname, Cfg),
                  {ok, State} end,
    meck:expect(rebar3_hex_config, update_auth_config, Fun),

    expect_local_password_prompt(Setup),
    Exps = [
            {any, string, "A", {return, "A"}},
            {"Proceed?", boolean, "Y", {returns, false}}
           ],
    expects_repo_config(Setup),
    expects_prompts(Exps),
	expects_output(default_publish_io(Setup));

setup_mocks_for(retire, #{username := Username, repo := Repo} = Setup) ->
    expects_repo_config(Setup),
    Fun = fun(Cfg, State) ->
                  Rname = maps:get(name, Repo),
                  Skey = maps:get(repo_key, Repo),
                  [Rname] = maps:keys(Cfg),
                  #{repo_key := Skey, username := Username} = maps:get(Rname, Cfg),
                  {ok, State} end,
    meck:expect(rebar3_hex_config, update_auth_config, Fun),
    expect_local_password_prompt(Setup);

setup_mocks_for(register, #{email := Email, repo := Repo} = Setup) ->
    expects_repo_config(Setup),
    expect_local_password_prompt(Setup),
    RepoName = maps:get(name, Repo),
    expects_registration_confirmation_output(RepoName, Email),
    expects_registration_output(),
    expects_user_registration_prompts(Setup);

setup_mocks_for(register_existing, Setup) ->
    expects_registration_output(),
    expect_local_password_prompt(Setup),
    expects_user_registration_prompts(Setup).


default_publish_io(#{selected_app := #{name := AppName}, repo := #{name := RepoName}}) ->
    BinAppName = rebar_utils:to_binary(AppName),
    CocStr = "Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct",
    [
     {"Select application(s):",[]},
     {"------------",[]},
     {"A) All",[]},
     {"Publishing ~ts ~ts to ~ts",[BinAppName,"0.1.0",RepoName]},
     {"  Description: ~ts",["An OTP application"]},
     {"  Dependencies:~n    ~ts", ["verl 1.1.1\n    hex_core 0.8.2"]},
     {"  Dependencies:~n    ~ts", ["verl ~>1.1.1\n    hex_core ~>0.8.2"]},
     {"  Included files:~n    ~ts", any},
     {"  Licenses: ~ts",["Apache 2.0"]},
     {"  Links:~n    ~ts",[[]]},
     {"  Build tools: ~ts",[["rebar3"]]},
     {"Be aware, you are publishing to the public Hexpm repository.",[]},
     {CocStr, []}
    ].

expects_registration_output() ->
    ExpectedInfo = "By registering an account on Hex.pm you accept all our"
    ++ " policies and terms of service found at https://hex.pm/policies\n",
    expects_output([ExpectedInfo, "Registering..."]).

expects_registration_confirmation_output(RepoName, Email) ->
    ReqInfo = "You are required to confirm your email to access your account, "
    ++ "a confirmation email has been sent to ~s",
    TokenInfo = "Then run `rebar3 hex auth -r ~ts` to create and configure api tokens locally.",
    expects_output([{TokenInfo, [RepoName]}, {ReqInfo, [Email]}]).

expects_repo_config(#{repo := Repo}) ->
    meck:expect(rebar3_hex_config, all_repos, fun(_) -> [Repo] end),
    meck:expect(rebar3_hex_config, repo, fun(_) -> {ok, Repo} end),
    meck:expect(rebar3_hex_config, repo, fun(_, <<"hexpm">>) -> {ok, test_utils:default_config()} end).

expects_parent_repos(#{repo := Repo}) ->
    meck:expect(rebar3_hex_config, parent_repos, fun(_) -> [Repo] end).

expects_update_auth_config( #{username := Username, repo := Repo}) ->
    BinUsername = rebar_utils:to_binary(Username),
    Fun = fun(Cfg, State) ->
                  Rname = maps:get(name, Repo),
                  Skey = maps:get(repo_key, Repo),
                  [Rname] = maps:keys(Cfg),
                  #{repo_key := Skey, username := BinUsername} = maps:get(Rname, Cfg),
                  {ok, State} end,
    meck:expect(rebar3_hex_config, update_auth_config, Fun).

expects_update_auth_config_for(RepoName) ->
    Fun = fun(Cfg, State) ->
                  case Cfg of
                      #{RepoName := _Repo} ->
                          {ok, State};
                      Got ->
                        meck:exception(error, {expected, RepoName, got, Got})
                    end
                  end,
    meck:expect(rebar3_hex_config, update_auth_config, Fun).


%% Helper for mocking rebar3_hex_io:say/2.
%% if an any atom is supplied for an argument list in the list of expected prompts
%% a pattern match is performed to test equality.
expects_output([{_T, _A} | _R] = OneOfs) ->
    meck:expect(rebar3_hex_io, say, fun(Templ, Args) ->
        Pred = fun(E) ->
            case E of
                {T, any} ->
                    case {Templ, Args} of
                        {T, _} ->
                            true;
                        _ ->
                            false
                    end;
                Other ->
                    Other == {Templ, Args}
            end
          end,

		  case lists:any(Pred, OneOfs) of
              true ->
                  ok;

              false ->
                  meck:exception(error, {expected, OneOfs, got, {Templ, Args}})
		  end
	end);

%% Helper for mocking rebar3_hex_io:say/1. Takes a list of expected arguments
%% that may given to rebar3_hex_io:say/1. If no expected are found in any given call
%% a meck:exception is thrown.
expects_output([_T | _R] = OneOfs)  ->
	meck:expect(rebar3_hex_io, say, fun(Arg) ->
		  case lists:member(Arg, OneOfs) of
              true ->
                  ok;

              false ->
                  meck:exception(error, {expected, OneOfs, got, Arg})
		  end
	end).

expects_user_registration_prompts(#{email := Email, username := Username}) ->
    BinEmail = binary_to_list(Email),
    BinUsername = binary_to_list(Username),
    Exps = [
            {"Email:", string, "", {returns, BinEmail}},
            {"Username:", string, "", {returns, BinUsername}}
           ],
	expects_prompts(Exps).

%% Helper for mocking rebar3_hex_io:ask/2. Expected prompts are in the form of :
%% {PromptStr, PromptType, PromptValue, {return, PromptValue}
%% An example would be :
%%
%% {"Proceed?", boolean, true, {return, true}}
%%
%% Where "Proceed?" is what the user would see, boolean is the prompt type, true is the default return value
%% and {return, true} is the value we want to respond to the prompt with.
-type prompt() :: string().
-type prompt_value()  :: string().
-type prompt_type() :: atom().
-type return_value() :: {return, prompt_value()}.
-type expected_prompt() :: {prompt(), prompt_type(), prompt_value(), return_value()}.
-spec expects_prompts([expected_prompt()]) -> any().
expects_prompts(OneOfs) ->
	 meck:expect(rebar3_hex_io, ask, fun(Prompt, Type, Str) ->
     Pred = fun(E) ->
            case E of
                {any, T, V, _Return} ->
                    case {Prompt, Type, Str} of
                        {_, T, V} ->
                            true;
                        _ ->
                            false
                    end;
                {P, T, V} ->
                    {P, T, V} == {Prompt, Type, Str};
                {P, T, V, _} ->
                    {P, T, V} == {Prompt, Type, Str}
            end
          end,

		  case lists:filter(Pred, OneOfs) of
              [{_Prompt, _Type, _Str, {returns, Val}}] ->
                  Val;

              [] ->
                  meck:exception(error, {expected, OneOfs, got, {Prompt, Type, Str}})
		  end
	end).

expect_local_password_prompt(#{password := Password, password_confirmation := PasswordConfirm}) ->
    Fun = fun(Arg) ->
            case Arg of
                <<"Account Password: ">> ->
                    Password;
                <<"Account Password (confirm): ">> ->
                    PasswordConfirm;
                <<"Local Password: ">> ->
                    Password;
                <<"Local Password (confirm): ">> ->
                    PasswordConfirm;
                <<"New local Password: ">> ->
                    Password;
                <<"New local Password (confirm): ">> ->
                    PasswordConfirm
            end
    end,
    meck:expect(rebar3_hex_io, get_password, Fun).

reset_mocks(Modules) ->
    meck:reset(Modules),
    [meck:delete(Module, Fun, Arity, false)
     || {Module, Fun, Arity} <- meck:expects(Modules, true)].
