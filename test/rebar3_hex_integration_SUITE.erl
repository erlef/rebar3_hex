-module(rebar3_hex_integration_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
    [sanity_check
     , decrypt_write_key_test
     , bad_command_test
     , docs_test
     , docs_auth_error_test
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
     , publish_org_test
     , publish_org_error_test
     , publish_org_requires_repo_arg_test
     , publish_error_test
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

%%%%%%%%%%%%%%%%%%
%%% Test Cases %%%
%%%%%%%%%%%%%%%%%%

sanity_check(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        {ok, {201, _Headers, Res}} = create_user(User, Pass, Email, Repo),
        ?assertEqual(User, maps:get(<<"username">>, Res)),
        ?assertEqual(Email, maps:get(<<"email">>, Res)),
        ?assert(maps:is_key(<<"inserted_at">>, Res)),
        ?assert(maps:is_key(<<"updated_at">>, Res))
    end.


docs_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm:valid">>,
                                        name => <<"hexpm:valid">>,
                                        write_key => WriteKey
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(docs, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        DocState = test_utils:mock_command({[{hex, docs}, {repo, "hexpm:valid"}], []}, Repo, State),
        {ok, NewState} = rebar_prv_edoc:do(DocState),
        ?assertMatch({ok, NewState}, rebar3_hex_docs:do(NewState))
    end.

docs_revert_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm:valid">>,
                                        name => <<"hexpm:valid">>,
                                        write_key => WriteKey
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(docs, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        DocState = test_utils:mock_command({[{hex, docs}, {revert, "0.1.1"}, {repo, "hexpm:valid"}], []}, Repo, State),
        {ok, NewState} = rebar_prv_edoc:do(DocState),
        ?assertMatch({ok, NewState}, rebar3_hex_docs:do(NewState))
    end.

docs_revert_auth_error_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"eh">>, <<"eh">>, <<"eh">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm:valid">>,
                                        name => <<"hexpm:valid">>,
                                        write_key => WriteKey,
                                        username => <<"eh">>
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(docs, {<<"eh">>, <<"foo@bar.baz">>, <<"eh">>, Repo}),
        DocState = test_utils:mock_command({[{hex, docs}, {revert, "0.1.1"}, {repo, "hexpm:valid"}], []}, Repo, State),
        ExpError = {error,{rebar3_hex_docs,{revert,{unauthorized,#{}}}}},
        {ok, NewState} = rebar_prv_edoc:do(DocState),
        ?assertMatch(ExpError, rebar3_hex_docs:do(NewState))
    end.

docs_auth_error_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"eh">>, <<"eh">>, <<"eh">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm:valid">>,
                                        name => <<"hexpm:valid">>,
                                        write_key => WriteKey,
                                        username => <<"eh">>
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(docs, {<<"eh">>, <<"foo@bar.baz">>, <<"eh">>, Repo}),
        DocState = test_utils:mock_command({[{hex, docs}, {repo, "hexpm:valid"}], []}, Repo, State),
        {ok, NewState} = rebar_prv_edoc:do(DocState),

        ExpError = {error,{rebar3_hex_docs,{publish,{unauthorized,#{}}}}},
        ?assertMatch(ExpError , rebar3_hex_docs:do(NewState))
    end.

docs_invalid_repo_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm:eh">>,
                                        name => <<"hexpm:eh">>,
                                        write_key => WriteKey,
                                        username => <<"eh">>
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(docs, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        DocState = test_utils:mock_command({[{hex, docs}, {repo, "hexpm:valid"}], []}, Repo, State),
        {ok, NewState} = rebar_prv_edoc:do(DocState),

        ExpError = {error,{rebar3_hex_docs,{not_valid_repo,"hexpm:valid"}}},
        ?assertMatch(ExpError , rebar3_hex_docs:do(NewState))
    end.

docs_no_write_key_test(Config) ->
    begin
        Repo = test_utils:repo_config(#{repo => <<"hexpm:valid">>,
                                        name => <<"hexpm:valid">>,
                                        write_key => undefined,
                                        username => <<"mr_pockets">>
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(docs, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        DocState = test_utils:mock_command({[{hex, docs}, {repo, "hexpm:valid"}], []}, Repo, State),
        {ok, NewState} = rebar_prv_edoc:do(DocState),

        ExpError = {error,{rebar3_hex_docs,no_write_key}},
        ?assertMatch(ExpError , rebar3_hex_docs:do(NewState))
    end.

%
% We test this specific function here as it requires a mock.
decrypt_write_key_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),

        WriteKey = <<"key">>,

        Username = <<"mr_pockets">>,
        LocalPassword = <<"special_shoes">>,

        WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),
        % This combo is one byte off in the IV
        BadKey = {<<21,112,99,26,160,67,34,190,25,135,85,235,132,185,94,88>>,
                  {<<230,231,88,0>>,<<216,113,132,52,197,237,230,15,200,76,130,195,236,21,203,77>>}},
        setup_mocks_for(decrypt_write_key, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        ?assertThrow({error,{rebar3_hex_user,bad_local_password}}, rebar3_hex_user:decrypt_write_key(<<"mr_pockets">>,
                                                                                                    BadKey)),
        ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, WriteKeyEncrypted))
    end.

register_user_test(_config) ->
    begin
        Repo = test_utils:repo_config(),
        Passwd = <<"special_shoes">>,
        setup_mocks_for(register, {<<"mr_pockets">>, <<"foo24@bar.baz">>, Passwd, Passwd, Repo}),
        State = test_utils:mock_command(["register"], Repo),
        ?assertMatch({ok, State}, rebar3_hex_user:do(State))
    end.

register_existing_user_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes1">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(register_existing, {<<"mr_pockets1">>, Email, Pass, Repo}),
        ExpReason1 = <<"email already in use">>,
        ExpErr1 = {error, {rebar3_hex_user, {registration_failure, ExpReason1}}},
        State = test_utils:mock_command(["register"], Repo),
        ?assertMatch(ExpErr1, rebar3_hex_user:do(State))
    end.

register_empty_password_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes2">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(register, {User, Email, <<>>, <<>>, Repo}),
        AuthState = test_utils:mock_command(["register"], Repo),
        % Should we be throwing in the function?
        ?assertMatch(error, rebar3_hex_user:do(AuthState))
    end.


register_error_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes2">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(register, {User, Email, Pass, Pass, Repo}),
        meck:expect(hex_api_user, create, fun(_,_,_,_) -> {error, meh} end),
        AuthState = test_utils:mock_command(["register"], Repo),
        % Should we be throwing in the function?
        ExpErr = {error,{rebar3_hex_user,{registration_failure,["meh"]}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState))
    end.

register_password_mismatch_test(_Config) ->
    begin
        User = <<"mr_pockets2">>,
        Pass = <<"special_shoes2">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(register, {User, Email, Pass, <<"special_shoes0">>, Repo}),
        AuthState = test_utils:mock_command(["register"], Repo),
        ExpErr = {error,{rebar3_hex_user,{error,"passwords do not match"}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState))
    end.

auth_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes1">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(first_auth, {User, Email, Pass, Pass, Repo}),
        AuthState = test_utils:mock_command(["auth"], Repo),
        ?assertMatch({ok, AuthState}, rebar3_hex_user:do(AuthState))
    end.

auth_bad_local_password_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes1">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(first_auth, {User, Email, Pass, <<"oops">>, Repo}),
        AuthState = test_utils:mock_command(["auth"], Repo),
        ?assertThrow({error,{rebar3_hex_user,no_match_local_password}}, rebar3_hex_user:do(AuthState))
    end.

auth_password_24_char_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes_shoes">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(first_auth, {User, Email, Pass, Pass, Repo}),
        AuthState = test_utils:mock_command(["auth"], Repo),
        ?assertMatch({ok, AuthState}, rebar3_hex_user:do(AuthState))
    end.

auth_password_32_char_test(_Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes_shoes_shoes">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(),
        create_user(User, Pass, Email, Repo),
        setup_mocks_for(first_auth, {User, Email, Pass, Pass, Repo}),
        AuthState = test_utils:mock_command(["auth"], Repo),
        ?assertMatch({ok, AuthState}, rebar3_hex_user:do(AuthState))
    end.

auth_unhandled_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        Pass = <<"special_shoes">>,
        setup_mocks_for(first_auth, {<<"mr_pockets">>, <<"foo@bar.baz">>, Pass, Pass, Repo}),
        %% TODO: Revise hex_api_model and hex_db so that we don't need to meck this
        meck:new([hex_api_key]),
        meck:expect(hex_api_key, add, fun(_,_,_) -> {ok, {500, #{}, #{<<"message">> => <<"eh?">>}}} end),
        AuthState = test_utils:mock_command(["auth"], Repo),
        ExpErr = {error,{rebar3_hex_user,{generate_key,<<"eh?">>}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState)),
        meck:unload([hex_api_key])
    end.

auth_error_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        Pass = <<"special_shoes">>,
        setup_mocks_for(first_auth, {<<"mr_pockets">>, <<"foo@bar.baz">>, Pass, Pass, Repo}),
        %% TODO: Revise hex_api_model and hex_db so that we don't need to meck this
        meck:new([hex_api_key]),
        meck:expect(hex_api_key, add, fun(_,_,_) -> {error, meh} end),
        AuthState = test_utils:mock_command(["auth"], Repo),
        ExpErr = {error,{rebar3_hex_user,{generate_key,["meh"]}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(AuthState)),
        meck:unload([hex_api_key])
    end.

whoami_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(whoami, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        WhoamiState = test_utils:mock_command(["whoami"], Repo),
        ?assertMatch({ok, WhoamiState}, rebar3_hex_user:do(WhoamiState))
    end.

whoami_unknown_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(whoami, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        WhoamiState = test_utils:mock_command(["whoami"], test_utils:repo_config(#{read_key => <<"eh?">>}) ),
        ExpErr = {error,{rebar3_hex_user,{whoami_failure,<<"huh?">>}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState))
    end.

% TODO: We should definitely handle this case in the code at this point.
whoami_unhandled_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(whoami, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        WhoamiState = test_utils:mock_command(["whoami"], test_utils:repo_config(#{read_key => <<"bad">>}) ),
        % TODO: We should handle this case gracefully, whoami/2
        ?assertError({case_clause, {ok, {500, _, #{<<"whoa">> := <<"mr.">>}}}}, rebar3_hex_user:do(WhoamiState))
    end.

whoami_api_error_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(whoami, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        meck:expect(hex_api_user, me, fun(_) -> {error, meh} end),
        WhoamiState = test_utils:mock_command(["whoami"], test_utils:repo_config(#{read_key => <<"!">>})),
        ExpErr = {error,{rebar3_hex_user,{whoami_failure,["meh"]}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState))
    end.

whoami_error_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(whoami, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        meck:expect(hex_api_user, me, fun(_) -> {error, meh} end),
        WhoamiState = test_utils:mock_command(["whoami"], test_utils:repo_config(#{read_key => <<"key">>})),
        ExpErr = {error,{rebar3_hex_user,{whoami_failure,["meh"]}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState))
    end.

whoami_not_authed_test(_Config) ->
    begin
        Repo = test_utils:repo_config(#{read_key => undefined}),
        Str = "Not authenticated as any user currently for this repository",
        meck:expect(rebar3_hex_io, say, fun(Arg) ->
                                          case Arg of
                                              Str ->
                                                  ok;
                                              _ ->
                                                  meck:exception(error, {expected, Str, got, Arg}),
                                                  ok
                                          end
                                  end),
        catch rebar3_hex_io:say(Str),
        WhoamiState = test_utils:mock_command(["whoami"], Repo),
        ExpErr = {error,"Not authenticated as any user currently for this repository"},
        ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState)),
        ?assert(meck:validate(rebar3_hex_io))
    end.

reset_password_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(reset_password, {"mr_pockets", <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        ResetState = test_utils:mock_command(["reset_password"], Repo),
        ?assertMatch({ok, ResetState}, rebar3_hex_user:do(ResetState))
    end.


reset_password_api_error_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(reset_password, {"eh?", <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        ResetState = test_utils:mock_command(["reset_password"], test_utils:repo_config(#{username => <<"eh?">>})),
        ExpErr = {error,{rebar3_hex_user,{reset_failure,<<"huh?">>}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(ResetState))
    end.

reset_password_unhandled_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(reset_password, {"bad", <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        ResetState = test_utils:mock_command(["reset_password"], test_utils:repo_config(#{username => <<"bad">>})),
        % TODO: We should handle this case gracefully
        ?assertError({case_clause, {ok, {500, _, #{<<"whoa">> := <<"mr.">>}}}}, rebar3_hex_user:do(ResetState))
    end.

reset_password_error_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(reset_password, {"mr_pockets", <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        meck:expect(hex_api_user, reset_password
                    , fun(_,_) -> {error, meh} end),
        WhoamiState = test_utils:mock_command(["reset_password"], test_utils:repo_config()),
        ExpErr = {error,{rebar3_hex_user,{reset_failure,["meh"]}}},
        ?assertMatch(ExpErr, rebar3_hex_user:do(WhoamiState))
    end.

deauth_test(Config) ->
    begin
        Repo = test_utils:repo_config(),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(deauth, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        DeauthState = test_utils:mock_command(["deauth"], Repo, State),
        ?assertMatch({ok, DeauthState}, rebar3_hex_user:do(DeauthState))
    end.

publish_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),

        Repo = test_utils:repo_config(#{repo => <<"hexpm:valid">>,
                                        name => <<"hexpm:valid">>,
                                        write_key => WriteKey
                                        }),
       {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(publish, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        PubState = test_utils:mock_command({[{hex, publish}, {repo, "hexpm:valid"}], []}, Repo, State),
        ?assertMatch({ok, PubState}, rebar3_hex_publish:do(PubState))
    end.


% TODO: We need to test publishing when a repo is only in rebar.config
% which is valid and works. Setting up the state is not clear though.
% -- Bryan
publish_org_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{ name => <<"hexpm:valid">>,
                                        repo => <<"hexpm:valid">>,
                                        write_key => WriteKey
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(publish, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        PubState = test_utils:mock_command({[{hex, publish}, {repo, "hexpm:valid"}], []}, Repo, State),
        ?assertMatch({ok, PubState}, rebar3_hex_publish:do(PubState))
    end.

publish_org_error_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{
                 repo => <<"hexpm:foo">>,
                 write_key => WriteKey
                }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(publish, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        PubState = test_utils:mock_command({[{hex, publish}, {repo, "hexpm:bar"}], []}, Repo, State),
        ?assertMatch(
           {error,{rebar3_hex_publish,{not_valid_repo,"hexpm:bar"}}}, rebar3_hex_publish:do(PubState))
    end.

publish_org_requires_repo_arg_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{ name => <<"hexpm:valid">>,
                                        repo => <<"hexpm:valid">>,
                                        write_key => WriteKey,
                                        parent => <<"hexpm">>
                                        }),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(publish, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        PubState = test_utils:mock_command({[{hex, publish}], []}, Repo, State),
        ?assertMatch({error,{rebar3_hex_publish,{required,repo}}}, rebar3_hex_publish:do(PubState))
    end.

publish_error_test(_Config) ->
    begin
        Repo = test_utils:repo_config(#{write_key => undefined, name => <<"hexpm:eh">>, repo => <<"hexpm:eh">>}),
        setup_mocks_for(publish, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        PubState = test_utils:mock_command({[{hex, publish}, {repo, "hexpm:eh"}], []}, Repo),
        ?assertMatch({error,{rebar3_hex_publish,no_write_key}}, rebar3_hex_publish:do(PubState))
    end.

key_list_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
       {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        ListState = test_utils:mock_command(["list"], Repo, State),
        ?assertMatch({ok, ListState}, rebar3_hex_key:do(ListState))
    end.

key_get_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
       {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        GetState = test_utils:mock_command(["fetch", "key"], Repo, State),
        ?assertMatch({ok, GetState}, rebar3_hex_key:do(GetState))
    end.

key_add_test(Config) ->
    begin
        %meck:unload([hex_api_key]),
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
       {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(key_mutation, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        {ok, AddState} = test_utils:new_mock_command(rebar3_hex_key, ["generate", "foo"], Repo, State),
        ?assertMatch({ok, AddState}, rebar3_hex_key:do(AddState))
    end.

key_delete_test(Config) ->
    begin
        %meck:unload([hex_api_key]),
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
       {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(key_mutation, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        {ok, AddState} = test_utils:new_mock_command(rebar3_hex_key, ["revoke", "key"], Repo, State),
        ?assertMatch({ok, AddState}, rebar3_hex_key:do(AddState))
    end.

key_delete_all_test(Config) ->
    begin
        %meck:unload([hex_api_key]),
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
       {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(key_mutation, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        {ok, AddState} = test_utils:new_mock_command(rebar3_hex_key, ["revoke", "--all"], Repo, State),
        ?assertMatch({ok, AddState}, rebar3_hex_key:do(AddState))
    end.

owner_add_test(Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes">>,
        Email = <<"foo@bar.baz">>,
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
        create_user(User, Pass, Email, Repo),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(owner, {User, Email, Pass, Repo}),
        AddState = test_utils:mock_command(["add", "truecoat", "wade@foo.bar"], Repo, State),
        ?assertMatch({ok, AddState}, rebar3_hex_owner:do(AddState))
    end.

owner_transfer_test(Config) ->
    begin
        WriteKey = rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>, <<"special_shoes">>, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
       {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(owner, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        AddState = test_utils:mock_command(["transfer", "truecoat", "gustafson_motors", "-r", "hexpm"], Repo, State),
        ?assertMatch({ok, AddState}, rebar3_hex_owner:do(AddState))
    end.

owner_remove_test(Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes">>,
        Email = <<"foo@bar.baz">>,
        WriteKey = rebar3_hex_user:encrypt_write_key(User, Pass, <<"key">>),
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>,
                                        write_key => WriteKey
                                        }),
        create_user(User, Pass, Email, Repo),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(owner, {User, Email, Pass, Repo}),
        RemoveState = test_utils:mock_command(["remove", "truecoat", "wade@foo.bar"], Repo, State),
        ?assertMatch({ok, RemoveState}, rebar3_hex_owner:do(RemoveState))
    end.

owner_list_test(Config) ->
    begin
        User = <<"mr_pockets">>,
        Pass = <<"special_shoes">>,
        Email = <<"foo@bar.baz">>,
        Repo = test_utils:repo_config(#{repo => <<"hexpm">>,
                                        name => <<"hexpm">>}),
        create_user(User, Pass, Email, Repo),
        {ok, _App, State} = test_utils:mock_app("valid", ?config(data_dir, Config), Repo),
        setup_mocks_for(owner, {User, Email, Pass, Repo}),
        ListState = test_utils:mock_command(["list", "truecoat"], Repo, State),
        ?assertMatch({ok, ListState}, rebar3_hex_owner:do(ListState))
    end.

bad_command_test(_Config) ->
    begin
        Repo = test_utils:repo_config(),
        setup_mocks_for(deauth, {<<"mr_pockets">>, <<"foo@bar.baz">>, <<"special_shoes">>, Repo}),
        BadcommandState = test_utils:mock_command(["bad_command"], Repo),
        ?assertThrow({error,{rebar3_hex_user,bad_command}}, rebar3_hex_user:do(BadcommandState))
    end.

%%%%%%%%%%%%%%%%%%
%%%  Helpers   %%%
%%%%%%%%%%%%%%%%%%

create_user(User, Pass, Email, Repo) ->
    hex_api_user:create(Repo, User, Pass, Email).

setup_mocks_for(decrypt_write_key, {_Username, _Email, Password, _Repo}) ->
    meck:expect(rebar3_hex_io, get_password, fun(_Arg) -> Password end);


setup_mocks_for(docs, {Username, _Email, Password, Repo}) ->
    meck:expect(rebar3_hex_config, update_auth_config, fun(Cfg, State) ->
                                                              Rname = maps:get(name, Repo),
                                                              Skey = maps:get(repo_key, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{repo_key := Skey, username := Username} = _ = maps:get(Rname, Cfg),
                                                              {ok, State} end),

    meck:expect(rebar3_hex_io, get_password, fun(_Arg) -> Password end),
    meck:expect(rebar3_hex_io, ask, fun(Prompt, Type, Str) ->
                                              case {Prompt, Type, Str} of
                                                {_, string, "A"} ->
                                                      "A";
                                                {"Proceed?", boolean, "Y"} ->
                                                      true
                                                end
                                    end),
    CocStr = "Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct",
    meck:expect(rebar3_hex_io, say, fun(Templ, Args) ->
                                      case {Templ, Args} of
                                          {"Select application(s):",[]} ->
                                              ok;
                                          {"------------",[]} ->
                                              ok;
                                          {"A) All",[]} ->
                                              ok;
                                          {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm:valid">>]} ->
                                              ok;
                                          {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm">>]} ->
                                              ok;
                                          {"  Description: ~ts",["An OTP application"]} ->
                                              ok;
                                          {"  Dependencies:~n    ~ts",[[]]} ->
                                              ok;
                                          {"  Included files:~n    ~ts", _Included} ->
                                              ok;
                                          {"  Licenses: ~ts",["Apache 2.0"]} ->
                                             ok;
                                          {"  Links:~n    ~ts",[[]]} ->
                                             ok;
                                          {"  Build tools: ~ts",[["rebar3"]]} ->
                                              ok;
                                          {"Be aware, you are publishing to the public Hexpm repository.",[]} ->
                                            ok;
                                          {CocStr, []} ->
                                            ok
                                      end
                              end);

setup_mocks_for(first_auth, {Username, Email, Password, PasswordConfirm, Repo}) ->
    AuthInfo =  "You have authenticated on Hex using your account password. "
    ++ "However, Hex requires you to have a local password that applies "
    ++ "only to this machine for security purposes. Please enter it.",
    meck:expect(rebar3_hex_config, update_auth_config, fun(Cfg, State) ->
                                                              Rname = maps:get(repo_name, Repo),
                                                              Skey = maps:get(repo_key, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{repo_key := Skey, username := Username} = _ = maps:get(Rname, Cfg),
                                                              {ok, State} end),
    expect_local_password_prompt(Password, PasswordConfirm),
    meck:expect(rebar3_hex_io, say, fun(Arg) ->
                                      case Arg of
                                          "Generating all keys..." ->
                                              ok;
                                          AuthInfo ->
                                              ok
                                      end
                              end),
    meck:expect(rebar3_hex_io, ask, fun(Prompt, string, "") ->
                                              case Prompt of
                                                  "Email:" ->
                                                      binary_to_list(Email);
                                                  "Username:" ->
                                                      binary_to_list(Username)
                                              end
                                      end);

setup_mocks_for(whoami, {Username, Email, _Password, #{name := Name} = _Repo}) ->
    meck:expect(rebar3_hex_io, say, fun(Str, Args) ->
                                      case {Str, Args} of
                                          {"~ts : ~ts (~ts)", [<<Name/binary>>, <<Username/binary>>, <<Email/binary>>]} ->
                                              ok
                                      end
                              end);

setup_mocks_for(reset_password, {Username, _Email, _Password, _Repo}) ->
    meck:expect(rebar3_hex_io, ask, fun(_Prompt, string, "") -> Username end),
    meck:expect(rebar3_hex_io, say, fun(Templ, Args) ->
                                      case {Templ, Args} of
                                          {"Email with reset link sent", []} ->
                                              ok
                                      end
                              end);

setup_mocks_for(deauth, {Username, _Email, _Password, #{name := RepoName} = _Repo}) ->
    meck:expect(rebar3_hex_config, update_auth_config, fun(#{RepoName := #{}}, State) ->
                                                              {ok, State} end),
    Templ1 = "User `~s` removed from the local machine. To authenticate again, run `rebar3 hex ",
    Templ2 = "user auth` or create a new user with `rebar3 hex user register`",
    ExpTempl = string:concat(Templ1, Templ2),
    meck:expect(rebar3_hex_io, say, fun(Templ, Args) ->
                                      case {Templ, Args} of
                                           {ExpTempl, [Username]} ->
                                              ok
                                      end
                              end);

setup_mocks_for(key_mutation, {_Username, _Email, Password, #{name := _RepoName} = _Repo}) ->
        meck:expect(rebar3_hex_io, get_password, fun(_Arg) -> Password end),
    ok;

setup_mocks_for(publish, {Username, _Email, Password, Repo}) ->
    meck:expect(rebar3_hex_config, update_auth_config, fun(Cfg, State) ->
                                                              Rname = maps:get(name, Repo),
                                                              Skey = maps:get(repo_key, Repo),
                                                              [Rname] = maps:keys(Cfg),
                                                              #{repo_key := Skey, username := Username} = _ = maps:get(Rname, Cfg),
                                                              {ok, State} end),

    meck:expect(rebar3_hex_io, get_password, fun(_Arg) -> Password end),
    meck:expect(rebar3_hex_io, ask, fun(Prompt, Type, Str) ->
                                              case {Prompt, Type, Str} of
                                                {_, string, "A"} ->
                                                      "A";
                                                {"Proceed?", boolean, "Y"} ->
                                                      true
                                                end
                                    end),
    CocStr = "Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct",
    meck:expect(rebar3_hex_io, say, fun(Templ, Args) ->
                                      case {Templ, Args} of
                                          {"Select application(s):",[]} ->
                                              ok;
                                          {"------------",[]} ->
                                              ok;
                                          {"A) All",[]} ->
                                              ok;
                                          {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm:valid">>]} ->
                                              ok;
                                          {"Publishing ~ts ~ts to ~ts",[<<"valid">>,"0.1.0",<<"hexpm">>]} ->
                                              ok;
                                          {"  Description: ~ts",["An OTP application"]} ->
                                              ok;
                                          {"  Dependencies:~n    ~ts",[[]]} ->
                                              ok;
                                          {"  Included files:~n    ~ts", _Included} ->
                                              ok;
                                          {"  Licenses: ~ts",["Apache 2.0"]} ->
                                             ok;
                                          {"  Links:~n    ~ts",[[]]} ->
                                             ok;
                                          {"  Build tools: ~ts",[["rebar3"]]} ->
                                              ok;
                                          {"Be aware, you are publishing to the public Hexpm repository.",[]} ->
                                            ok;
                                          {CocStr, []} ->
                                            ok
                                      end
                              end);


setup_mocks_for(owner, {_Username, _Email, Password, _Repo}) ->
    meck:expect(rebar3_hex_io, get_password, fun(_Arg) -> Password end),
    meck:expect(rebar3_hex_io, say, fun(Templ, Args) ->
                                      case {Templ, Args} of
                                          {_, [_UserOrOrg, _Package]} -> %% add/remove
                                            ok;
                                          {"~s",[[]]} ->  %% list
                                            ok
                                      end
                              end);

setup_mocks_for(register, {Username, Email, Password, PasswordConfirm, Repo}) ->
    ExpectedInfo = "By registering an account on Hex.pm you accept all our"
    ++ " policies and terms of service found at https://hex.pm/policies\n",
    meck:expect(rebar3_hex_config, update_auth_config, fun(_Cfg, State) -> {ok, State} end),
    expect_local_password_prompt(Password, PasswordConfirm),
    ReqInfo = "You are required to confirm your email to access your account, "
    ++ "a confirmation email has been sent to ~s",
    TokenInfo = "Then run `rebar3 hex auth -r ~ts` to create and configure api tokens locally.",
    RepoName = maps:get(name, Repo),
    meck:expect(rebar3_hex_io, say, fun(Str, Args) ->
                                      case {Str, Args} of
                                          {TokenInfo, [RepoName]} ->
                                              ok;
                                          {ReqInfo, [Email]} ->
                                              ok
                                      end
                              end),
    meck:expect(rebar3_hex_io, say, fun(Arg) ->
                                      case Arg of
                                          ExpectedInfo ->
                                              ok;
                                          "Registering..." ->
                                              ok
                                      end
                              end),
    meck:expect(rebar3_hex_io, ask, fun(Prompt, string, "") ->
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
    meck:expect(rebar3_hex_io, get_password, fun(_Arg) -> Password end),
    meck:expect(rebar3_hex_io, say, fun(Arg) ->
                                      case Arg of
                                          ExpectedInfo ->
                                              ok;
                                          "Registering..." ->
                                              ok
                                      end
                              end),
    meck:expect(rebar3_hex_io, ask, fun(Prompt, string, "") ->
                                              case Prompt of
                                                  "Email:" ->
                                                      binary_to_list(Email);
                                                  "Username:" ->
                                                      binary_to_list(Username)
                                              end
                                      end).

expect_local_password_prompt(Password, PasswordConfirm) ->
    meck:expect(rebar3_hex_io, get_password, fun(Arg) ->
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
                                                end).


reset_mocks(Modules) ->
    meck:reset(Modules),
    [meck:delete(Module, Fun, Arity, false)
     || {Module, Fun, Arity} <- meck:expects(Modules, true)].
