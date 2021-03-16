-module(rebar3_hex_integration_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
    [ sanity_check
    , decrypt_write_key_test
    , bad_command_test
    , docs_test
    , docs_auth_error_test
    , docs_dir_error_test
    , docs_invalid_repo_test
    , docs_no_write_key_test
    , docs_revert_test
    , docs_revert_auth_error_test
    , reset_password_test
    , reset_password_error_test
    , reset_password_unhandled_test
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
    , whoami_unhandled_test
    , deauth_test
    , publish_test
    , publish_replace_test
    , publish_revert_test
    , publish_org_test
    , publish_org_error_test
    , publish_org_requires_repo_arg_test
    , publish_error_test
    , publish_unauthorized_test
    , key_list_test
    , key_get_test
    , key_add_test
    , key_delete_test
    , key_delete_all_test
    , owner_add_test
    , owner_transfer_test
    , owner_list_test
    , owner_remove_test].

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

-define(default_repo_config, #{repo => ?default_repo,
                                    name => ?default_repo,
                                    username => ?default_username
                              }).

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

docs_test(Config) ->
    P = #{app => "valid", mocks => [docs]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    Command = ["docs"],
    RepoConfig =  [{repos,[Repo]}],
    {ok, DocState} = test_utils:mock_command(rebar3_hex_docs, Command, RepoConfig, State),
    {ok, NewState} = rebar_prv_edoc:do(DocState),
    ?assertMatch({ok, NewState}, rebar3_hex_docs:do(NewState)).

docs_dir_error_test(Config) ->
    P = #{app => "valid", mocks => [docs]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    Command = ["docs"],
    RepoConfig = [{repos,[Repo]}],
    {ok, NewState} = test_utils:mock_command(rebar3_hex_docs, Command, RepoConfig, State),
    ?assertThrow(rebar_abort, rebar3_hex_docs:do(NewState)).

docs_revert_test(Config) ->
    P = #{app => "valid", mocks => [docs]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    Command = ["revert", "0.1.1"],
    RepoConfig =  [{repos,[Repo]}],
    {ok, DocState} = test_utils:mock_command(rebar3_hex_docs, Command, RepoConfig, State),
    {ok, NewState} = rebar_prv_edoc:do(DocState),
    ?assertMatch({ok, NewState}, rebar3_hex_docs:do(NewState)).

docs_revert_auth_error_test(Config) ->
    P = #{app => "valid",
          mocks => [docs],
          repo_config => #{username => <<"eh">>},
          username => <<"eh">>,
          password => <<"eh">>,
          key_phrase => <<"eh">>
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    Command = ["revert", "0.1.1"],
    RepoConfig = [{repos,[Repo]}],
    {ok, DocState} = test_utils:mock_command(rebar3_hex_docs, Command, RepoConfig, State),
    %ExpError = {error,{rebar3_hex_docs,{revert,{unauthorized,#{}}}}},
    ExpError = {error,{rebar3_hex_docs,{publish,{error,#{}}}}},
    {ok, NewState} = rebar_prv_edoc:do(DocState),
    ?assertMatch(ExpError, rebar3_hex_docs:do(NewState)).

docs_auth_error_test(Config) ->
    P = #{app => "valid",
          mocks => [docs],
          repo_config => #{username => <<"eh">>},
          username => <<"eh">>,
          password => <<"eh">>,
          key_phrase => <<"eh">>
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    Command = [],
    RepoConfig =  [{repos,[Repo]}],
    {ok, DocState} = test_utils:mock_command(rebar3_hex_docs, Command, RepoConfig, State),
    {ok, NewState} = rebar_prv_edoc:do(DocState),
    ExpError = {error,{rebar3_hex_docs,{publish,{error,#{}}}}},
    ?assertMatch(ExpError , rebar3_hex_docs:do(NewState)).

docs_invalid_repo_test(Config) ->
    P = #{app => "valid",
          mocks => [docs],
          repo_config => #{repo => <<"hexpm:eh">>,
                           name => <<"hexpm:eh">>,
                           username => <<"eh">>
                          }

         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig =  [{repos,[Repo]}],
    {ok, DocState} = test_utils:mock_command(rebar3_hex_docs, ["-r", "hexpm:valid"], RepoConfig, State),
    {ok, NewState} = rebar_prv_edoc:do(DocState),

    ExpError = {error,{rebar3_hex_docs,{not_valid_repo,"hexpm:valid"}}},
    ?assertMatch(ExpError , rebar3_hex_docs:do(NewState)).

docs_no_write_key_test(Config) ->
    P = #{app => "valid",
          mocks => [docs],
          repo_config => #{write_key => undefined}
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig =  [{repos,[Repo]}],
    {ok, DocState} = test_utils:mock_command(rebar3_hex_docs, [], RepoConfig, State),
    {ok, NewState} = rebar_prv_edoc:do(DocState),

    ExpError = {error,{rebar3_hex_docs,no_write_key}},
    ?assertMatch(ExpError , rebar3_hex_docs:do(NewState)).

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
                                         LocalPassword, repo => Repo}),

    ?assertThrow({error,{rebar3_hex_user,bad_local_password}}, rebar3_hex_user:decrypt_write_key(<<"mr_pockets">>,
                                                                                                 BadKey)),
    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, WriteKeyEncrypted)).

register_user_test(Config) ->
    P = #{app => "valid",
          mocks => [register],
          username => <<"jlundegaard">>,
          email => <<"jlundegaard@gustafson-mortors.com">>
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig =  [{repos,[Repo]}],
    {ok, RegState} = test_utils:mock_command(rebar3_hex_user, ["register"], RepoConfig, State),
    ?assertMatch({ok, RegState}, rebar3_hex_user:do(RegState)).

register_existing_user_test(Config) ->
    P = #{app => "valid",
          mocks => [register],
          username => <<"not_taken">>,
          email => ?default_email
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    ExpErr1 = {error, {rebar3_hex_user, {registration_failure, <<"email already in use">>}}},
    RepoConfig = [{repos,[Repo]}],
    {ok, RegState} = test_utils:mock_command(rebar3_hex_user, ["register"], RepoConfig, State),
    ?assertMatch(ExpErr1, rebar3_hex_user:do(RegState)).

register_empty_password_test(Config) ->
    P = #{app => "valid",
          mocks => [register],
          password => <<>>
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig =  [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["register"], RepoConfig, State),

    ?assertMatch(error, rebar3_hex_user:do(AuthState)).

register_error_test(Config) ->
    meck:expect(hex_api_user, create, fun(_,_,_,_) -> {error, meh} end),
    P = #{app => "valid", mocks => [register]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["register"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{registration_failure,["meh"]}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState)).

register_password_mismatch_test(Config) ->
    P = #{app => "valid", mocks => [register], password_confirmation => <<"special_shoes0">>},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["register"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{error,"passwords do not match"}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState)).

auth_test(Config) ->
    P = #{app => "valid", mocks => [first_auth]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["auth"], RepoConfig, State),
    ?assertMatch({ok, AuthState}, rebar3_hex_user:do(AuthState)).

auth_bad_local_password_test(Config) ->
    P = #{app => "valid", mocks => [first_auth], password_confirmation => <<"oops">>},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["auth"], RepoConfig, State),
    ?assertThrow({error,{rebar3_hex_user,no_match_local_password}}, rebar3_hex_user:do(AuthState)).

auth_password_24_char_test(Config) ->
    Pass = <<"special_shoes_shoes">>,
    P = #{app => "valid", mocks => [first_auth], password => Pass, password_confirmation => Pass},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["auth"], RepoConfig, State),
    ?assertMatch({ok, AuthState}, rebar3_hex_user:do(AuthState)).

auth_password_32_char_test(Config) ->
    Pass = <<"special_shoes_shoes_shoes">>,
    P = #{app => "valid", mocks => [first_auth], password => Pass, password_confirmation => Pass},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["auth"], RepoConfig, State),
    ?assertMatch({ok, AuthState}, rebar3_hex_user:do(AuthState)).

auth_unhandled_test(Config) ->
    %% TODO: Revise hex_api_model and hex_db so that we don't need to meck this
    meck:new([hex_api_key]),
    MeckReturn = {ok, {500, #{}, #{<<"message">> => <<"eh?">>}}},
    meck:expect(hex_api_key, add, fun(_,_,_) -> MeckReturn end),

    P = #{app => "valid", mocks => [first_auth]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["auth"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{generate_key,<<"eh?">>}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState)),
    meck:unload([hex_api_key]).

auth_error_test(Config) ->
    %% TODO: Revise hex_api_model and hex_db so that we don't need to meck this
    meck:new([hex_api_key]),
    meck:expect(hex_api_key, add, fun(_,_,_) -> {error, meh} end),

    P = #{app => "valid", mocks => [first_auth]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, AuthState} = test_utils:mock_command(rebar3_hex_user, ["auth"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{generate_key,["meh"]}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState)),
    meck:unload([hex_api_key]).

whoami_test(Config) ->
    P = #{app => "valid", mocks => [whoami]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, WhoamiState} = test_utils:mock_command(rebar3_hex_user, ["whoami"], RepoConfig, State),

    ?assertMatch({ok, WhoamiState}, rebar3_hex_user:do(WhoamiState)).

whoami_unknown_test(Config) ->
    P = #{app => "valid", mocks => [whoami], repo_config => #{read_key => <<"eh?">>}},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, WhoamiState} = test_utils:mock_command(rebar3_hex_user, ["whoami"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{whoami_failure,<<"huh?">>}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState)).

% TODO: We should definitely handle this case in the code at this point.
whoami_unhandled_test(_Config) ->
    %% Double check this is still relevent
    ok.
% Repo = test_utils:repo_config(),
% setup_mocks_for(whoami, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
% Repo1 = test_utils:repo_config(#{read_key => <<"eh?">>}),
% {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo1),
% {ok, WhoamiState } = test_utils:mock_command(rebar3_hex_user, ["whoami"], Repo1, State),
% % TODO: We should handle this case gracefully, whoami/2
% ?assertError({case_clause, {ok, {500, _, #{<<"whoa">> := <<"mr.">>}}}}, rebar3_hex_user:do(WhoamiState))

whoami_api_error_test(Config) ->
    meck:expect(hex_api_user, me, fun(_) -> {error, meh} end),
    P = #{app => "valid", mocks => [whoami], repo_config => #{read_key => <<"!">>}},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, WhoamiState} = test_utils:mock_command(rebar3_hex_user, ["whoami"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{whoami_failure,["meh"]}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState)).

whoami_error_test(Config) ->
    meck:expect(hex_api_user, me, fun(_) -> {error, meh} end),
    P = #{app => "valid", mocks => [whoami]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, WhoamiState} = test_utils:mock_command(rebar3_hex_user, ["whoami"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{whoami_failure,["meh"]}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState)).

whoami_not_authed_test(Config) ->
    Str = "Not authenticated as any user currently for this repository",
    expects_output([Str]),
    P = #{app => "valid", mocks => [whoami], repo_config => #{read_key => undefined}},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, WhoamiState } = test_utils:mock_command(rebar3_hex_user, ["whoami"], RepoConfig, State),

    ExpErr = {error,"Not authenticated as any user currently for this repository"},
    ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState)),
    ?assert(meck:validate(rebar3_hex_io)).

reset_password_test(Config) ->
    P = #{app => "valid", mocks => [reset_password], username => "mr_pockets"},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, ResetState} = test_utils:mock_command(rebar3_hex_user, ["reset_password"], RepoConfig, State),

    ?assertMatch({ok, ResetState}, rebar3_hex_user:do(ResetState)).


reset_password_api_error_test(Config) ->
    P = #{app => "valid", mocks => [reset_password], username => "eh?", repo_config => #{username => "eh?"}},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, ResetState} = test_utils:mock_command(rebar3_hex_user, ["reset_password"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{reset_failure,<<"huh?">>}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(ResetState)).

reset_password_unhandled_test(_Config) ->
    %% Make sure this is still relevant
    %%
    ok.
% Repo = test_utils:repo_config(#{username => <<"bad">>}),
% setup_mocks_for(reset_password, {"bad", <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
% {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
% {ok, ResetState} = test_utils:mock_command(rebar3_hex_user, ["reset_password"], Repo, State),
% % TODO: We should handle this case gracefully
% ?assertError({case_clause, {ok, {500, _, #{<<"whoa">> := <<"mr.">>}}}}, rebar3_hex_user:do(ResetState))

reset_password_error_test(Config) ->
    meck:expect(hex_api_user, reset_password, fun(_,_) -> {error, meh} end),
    P = #{app => "valid", mocks => [reset_password], username => "mr_pockets"},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, ResetState} = test_utils:mock_command(rebar3_hex_user, ["reset_password"], RepoConfig, State),

    ExpErr = {error,{rebar3_hex_user,{reset_failure,["meh"]}}},
    ?assertMatch(ExpErr, rebar3_hex_user:do(ResetState)).

deauth_test(Config) ->
    P = #{app => "valid", mocks => [deauth]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, DeauthState} = test_utils:mock_command(rebar3_hex_user, ["deauth"], RepoConfig, State),

    ?assertMatch({ok, DeauthState}, rebar3_hex_user:do(DeauthState)).

publish_test(Config) ->
    P = #{app => "valid", mocks => [publish]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, [], RepoConfig, State),
    {ok, _} = rebar_prv_edoc:do(PubState),

    ?assertMatch({ok, PubState}, rebar3_hex_publish:do(PubState)).

%% TODO: This test currently is merely to see if we can handle the --replace switch
%% In order for the test to be more meaningful we need to update the hex_api_model to keep
%% track of packages that have been published and when, further we need to provide
%% a package add function on hex_api_model which takes a package name, published at timestamp, etc.
%% so we can test the sad paths (i.e., you can not replace a package after N seconds)
publish_replace_test(Config) ->
    P = #{app => "valid", mocks => [publish]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, ["--replace"], RepoConfig, State),
    {ok, _} = rebar_prv_edoc:do(PubState),

    ?assertMatch({ok, PubState}, rebar3_hex_publish:do(PubState)).

publish_revert_test(Config) ->
    P = #{app => "valid", mocks => [publish_revert], version => "1.0.0"},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, ["--revert", "1.0.0", "--package", "valid"], RepoConfig, State),

    ?assertMatch({ok, PubState}, rebar3_hex_publish:do(PubState)).

% TODO: We need to test publishing when a repo is only in rebar.config
% which is valid and works. Setting up the state is not clear though.
% -- Bryan
publish_org_test(Config) ->
    P = #{app => "valid",
          mocks => [publish],
          repo_config => #{repo => <<"hexpm:valid">>,
                           name => <<"hexpm:valid">>
                          }
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, ["-r", "hexpm:valid"], RepoConfig, State),
    {ok, _} = rebar_prv_edoc:do(PubState),

    ?assertMatch({ok, PubState}, rebar3_hex_publish:do(PubState)).

publish_org_error_test(Config) ->
    P = #{app => "valid", mocks => [publish], repo_config => #{repo => <<"hexpm:foo">>, name => <<"hexpm:foo">>}},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, ["-r", "hexpm:bar"], RepoConfig, State),

    ExpError = {error,{rebar3_hex_publish,{not_valid_repo,"hexpm:bar"}}},
    ?assertMatch(ExpError, rebar3_hex_publish:do(PubState)).

publish_org_requires_repo_arg_test(Config) ->
    P = #{app => "valid", mocks => [publish], repo_config => #{repo => <<"hexpm:valid">>, name => <<"hexpm:valid">>}},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, [], RepoConfig, State),
    ?assertMatch({error,{rebar3_hex_publish,{required,repo}}}, rebar3_hex_publish:do(PubState)).

publish_error_test(Config) ->
    P = #{app => "valid", mocks => [publish], repo_config => #{write_key => undefined}},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, [], RepoConfig, State),

    ?assertMatch({error,{rebar3_hex_publish,no_write_key}}, rebar3_hex_publish:do(PubState)).

publish_unauthorized_test(Config) ->
    WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"unauthorized">>),
    P = #{app => "valid",
          mocks => [publish],
          repo_config => #{write_key => WriteKey, repo => <<"hexpm:valid">>, name => <<"hexpm:valid">>}
         },
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, PubState} = test_utils:mock_command(rebar3_hex_publish, ["-r", "hexpm:valid"], RepoConfig, State),
    Exp = {error,
           {rebar3_hex_publish,
            {publish,
             {error,
              #{<<"message">> =>
                <<"account not authorized for this action">>}}}}},
    ?assertMatch(Exp, rebar3_hex_publish:do(PubState)).

key_list_test(Config) ->
    P = #{app => "valid", mocks => []},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, ListState} = test_utils:mock_command(rebar3_hex_key, ["list"], RepoConfig, State),

    ?assertMatch({ok, ListState}, rebar3_hex_key:do(ListState)).

key_get_test(Config) ->
    P = #{app => "valid", mocks => []},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, GetState} = test_utils:mock_command(rebar3_hex_key, ["fetch", "-k", "key"], RepoConfig, State),

    ?assertMatch({ok, GetState}, rebar3_hex_key:do(GetState)).

key_add_test(Config) ->
    P = #{app => "valid", mocks => [key_mutation]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, AddState} = test_utils:mock_command(rebar3_hex_key, ["generate", "-k", "foo"], RepoConfig, State),

    ?assertMatch({ok, AddState}, rebar3_hex_key:do(AddState)).

key_delete_test(Config) ->
    P = #{app => "valid", mocks => [key_mutation]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, AddState} = test_utils:mock_command(rebar3_hex_key, ["revoke", "-k", "key"], RepoConfig, State),

    ?assertMatch({ok, AddState}, rebar3_hex_key:do(AddState)).

key_delete_all_test(Config) ->
    P = #{app => "valid", mocks => [key_mutation]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    RepoConfig = [{repos,[Repo]}],
    {ok, AddState} = test_utils:mock_command(rebar3_hex_key, ["revoke", "--all"], RepoConfig, State),

    ?assertMatch({ok, AddState}, rebar3_hex_key:do(AddState)).

owner_add_test(Config) ->
    P = #{app => "valid", mocks => [owner]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    Command = ["add", "truecoat", "wade@foo.bar"],
    RepoConfig = [{repos,[Repo]}],
    {ok, AddState} = test_utils:mock_command(rebar3_hex_owner, Command, RepoConfig, State),

    ?assertMatch({ok, AddState}, rebar3_hex_owner:do(AddState)).

owner_transfer_test(Config) ->
    P = #{app => "valid", mocks => [owner]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    Command = ["transfer", "truecoat", "gustafson_motors", "-r", "hexpm"],
    RepoConfig = [{repos,[Repo]}],
    {ok, AddState} = test_utils:mock_command(rebar3_hex_owner, Command, RepoConfig, State),

    ?assertMatch({ok, AddState}, rebar3_hex_owner:do(AddState)).

owner_remove_test(Config) ->
    P = #{app => "valid", mocks => [owner]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    Command = ["remove", "truecoat", "wade@foo.bar"],
    RepoConfig = [{repos,[Repo]}],
    {ok, RemoveState} = test_utils:mock_command(rebar3_hex_owner, Command, RepoConfig, State),

    ?assertMatch({ok, RemoveState}, rebar3_hex_owner:do(RemoveState)).

owner_list_test(Config) ->
    P = #{app => "valid", mocks => [owner]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    create_user(?default_username, ?default_password, ?default_email, Repo),
    Command = ["list", "truecoat"],
    RepoConfig = [{repos,[Repo]}],
    {ok, ListState} = test_utils:mock_command(rebar3_hex_owner, Command, RepoConfig, State),

    ?assertMatch({ok, ListState}, rebar3_hex_owner:do(ListState)).

bad_command_test(Config) ->
    P = #{app => "valid", mocks => [deauth]},
    {ok, #{rebar_state := State, repo := Repo}} = setup_state(P, Config),
    Command = ["bad_command"],
    RepoConfig = [{repos,[Repo]}],
    {ok, BadcommandState} = test_utils:mock_command(rebar3_hex_user, Command, RepoConfig, State),

    ?assertThrow({error,{rebar3_hex_user,bad_command}}, rebar3_hex_user:do(BadcommandState)).

%%%%%%%%%%%%%%%%%%
%%%  Helpers   %%%
%%%%%%%%%%%%%%%%%%


%% setup_state/2 takes a set of parameters and a CT config
%% The with param is used to setup predefined mock scenarios. May be an empty list if no mocks are required.
setup_state(P, Config) ->
    Params = maps:merge(?default_params, P),
    #{app := AppName,
      mocks := Mocks,
      username := Username,
      password := Password,
      password_confirmation := _PasswordConfirm,
      key_phrase := KeyPhrase,
      email := _Email}            = Params,

    WriteKey = maps:get(write_key, Params, rebar3_hex_user:encrypt_write_key(Username, Password, KeyPhrase)),
    DefRepoConfig = ?default_repo_config,
    ParamRepoConfig =  maps:get(repo_config, Params, DefRepoConfig),
    RepoConfig = maps:merge(DefRepoConfig#{write_key => WriteKey}, ParamRepoConfig),
    Repo = test_utils:repo_config(RepoConfig),

    {ok, App, State} = test_utils:mock_app(AppName, ?config(data_dir, Config), Repo),

    %% Make sure there is no leftover generated doc directory
    ok = rebar_file_utils:rm_rf(filename:join(rebar_app_info:dir(App), "doc")),

    Setup = Params#{ repo => Repo,
                     app_state => App,
                     rebar_state => State
                   },

    lists:foreach(fun(W) -> setup_mocks_for(W, Setup) end, Mocks),

    {ok, Setup}.

create_user(User, Pass, Email, Repo) ->
    hex_api_user:create(Repo, User, Pass, Email).

setup_mocks_for(decrypt_write_key, #{password := Password}) ->
    expect_local_password_prompt(Password, Password);

setup_mocks_for(org_auth, #{repo := Repo}) ->
    meck:expect(rebar3_hex_config, update_auth_config, fun(Cfg, _State) ->
                                                              Rname = maps:get(name, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{auth_key := <<"key">>} = _ = maps:get(Rname, Cfg),
                                                              ok end);

setup_mocks_for(docs, #{password := Password, password_confirmation := PasswordConfirm, repo := Repo}) ->
    meck:expect(rebar3_hex_config, update_auth_config, fun(Cfg, State) ->
                                                              Rname = maps:get(name, Repo),
                                                              Skey = maps:get(repo_key, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{repo_key := Skey} = _ = maps:get(Rname, Cfg),
                                                              {ok, State} end),

    expect_local_password_prompt(Password, PasswordConfirm),
	expects_prompts([{any, string, "A", {returns, "A"}}, {"Proceed?", boolean, "Y", {returns, true}}]),

    CocStr = "Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct",
    Expects = [
               {"Select application(s):",[]},
               {"------------",[]},
               {"A) All",[]},
               {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm:valid">>]},
               {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm">>]},
               {"  Description: ~ts",["An OTP application"]},
               {"  Dependencies:~n    ~ts",[[]]},
               {"  Included files:~n    ~ts", any},
               {"  Licenses: ~ts",["Apache 2.0"]},
               {"  Links:~n    ~ts",[[]]},
               {"  Build tools: ~ts",[["rebar3"]]},
               {"Be aware, you are publishing to the public Hexpm repository.",[]},
               {CocStr, []}
              ],
    expects_output(Expects);

setup_mocks_for(first_auth, #{username := Username,
                              email := Email,
                              password := Password,
                              password_confirmation := PasswordConfirm,
                              repo := Repo}) ->

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
    expect_local_password_prompt(Password, PasswordConfirm),
    expects_output(["Generating all keys...", AuthInfo]),
    expects_user_registration_prompts(Email, Username);

setup_mocks_for(owner, #{password := Password}) ->
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
                          repo := #{name := Name}}) ->

    Args =  [<<Name/binary>>, <<Username/binary>>, <<Email/binary>>],
    Expects = [{"~ts : ~ts (~ts)", Args}],
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

setup_mocks_for(key_mutation, #{password := Password, password_confirmation := PasswordConfirm}) ->
    expect_local_password_prompt(Password, PasswordConfirm);

setup_mocks_for(publish, #{username := Username,
                           password := Password,
                           password_confirmation := PasswordConfirm,
                           repo := Repo}) ->

    Fun = fun(Cfg, State) ->
                  Rname = maps:get(name, Repo),
                  Skey = maps:get(repo_key, Repo),
                  [Rname] = maps:keys(Cfg),
                  #{repo_key := Skey, username := Username} = maps:get(Rname, Cfg),
                  {ok, State} end,
    meck:expect(rebar3_hex_config, update_auth_config, Fun),

    expect_local_password_prompt(Password, PasswordConfirm),
    Exps = [
            {any, string, "A", {return, "A"}},
            {"Proceed?", boolean, "Y", {returns, true}}
           ],
    expects_prompts(Exps),
    CocStr = "Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct",
    OneOfs = [{"Select application(s):",[]},
              {"------------",[]},
              {"A) All",[]},
              {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm:valid">>]},
              {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm">>]},
              {"  Description: ~ts",["An OTP application"]},
              {"  Dependencies:~n    ~ts",[[]]},
              {"  Included files:~n    ~ts", any},
              {"  Licenses: ~ts",["Apache 2.0"]},
              {"  Links:~n    ~ts",[[]]},
              {"  Build tools: ~ts",[["rebar3"]]},
              {"Be aware, you are publishing to the public Hexpm repository.",[]},
              {CocStr, []}],
		expects_output(OneOfs);

setup_mocks_for(publish_revert, #{username := Username,
                           password := Password,
                           password_confirmation := PasswordConfirm,
                           version := Vsn,
                           repo := Repo}) ->

    Fun = fun(Cfg, State) ->
                  Rname = maps:get(name, Repo),
                  Skey = maps:get(repo_key, Repo),
                  [Rname] = maps:keys(Cfg),
                  #{repo_key := Skey, username := Username} = maps:get(Rname, Cfg),
                  {ok, State} end,
    meck:expect(rebar3_hex_config, update_auth_config, Fun),

    expect_local_password_prompt(Password, PasswordConfirm),
    Exps = [
            {[65,108,115,111,32,100,101,108,101,116,101,32,116,97,103,32,118,Vsn,
              63],
             boolean,"N", {returns, false}}
           ],
    expects_prompts(Exps);

setup_mocks_for(register, #{username := Username,
                            email := Email,
                            password := Password,
                            password_confirmation := PasswordConfirm,
                            repo := Repo}) ->

    expect_local_password_prompt(Password, PasswordConfirm),
    RepoName = maps:get(name, Repo),
    expects_registration_confirmation_output(RepoName, Email),
    expects_registration_output(),
    expects_user_registration_prompts(Email, Username);

setup_mocks_for(register_existing, #{username := Username,
                                     email := Email,
                                     password := Password}) ->

    expects_registration_output(),
    expect_local_password_prompt(Password, Password),
    expects_user_registration_prompts(Email, Username).

expects_registration_output() ->
    ExpectedInfo = "By registering an account on Hex.pm you accept all our"
    ++ " policies and terms of service found at https://hex.pm/policies\n",
    expects_output([ExpectedInfo, "Registering..."]).

expects_registration_confirmation_output(RepoName, Email) ->
    ReqInfo = "You are required to confirm your email to access your account, "
    ++ "a confirmation email has been sent to ~s",
    TokenInfo = "Then run `rebar3 hex auth -r ~ts` to create and configure api tokens locally.",
    expects_output([{TokenInfo, [RepoName]}, {ReqInfo, [Email]}]).

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

expects_user_registration_prompts(Email, Username) ->
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

expect_local_password_prompt(Password, PasswordConfirm) ->
    Fun = fun(Arg) ->
            case Arg of
                <<"Account Password: ">> ->
                    Password;
                <<"Account Password (confirm): ">> ->
                    PasswordConfirm;
                <<"Local Password: ">> ->
                    Password;
                <<"Local Password (confirm): ">> ->
                    PasswordConfirm
            end
    end,
    meck:expect(rebar3_hex_io, get_password, Fun).

reset_mocks(Modules) ->
    meck:reset(Modules),
    [meck:delete(Module, Fun, Arity, false)
     || {Module, Fun, Arity} <- meck:expects(Modules, true)].
