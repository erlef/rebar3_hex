-module(rebar3_hex_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [encrypt_decrypt_write_key, publish_invalid_semver].

encrypt_decrypt_write_key(_Config) ->
    WriteKey = <<"abc1234">>,

    Username = <<"user">>,
    LocalPassword = <<"password">>,

    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    ?assertThrow({error,{rebar3_hex_user,bad_local_password}},
                 rebar3_hex_user:decrypt_write_key(Username, <<"wrong password">>, WriteKeyEncrypted)),

    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, LocalPassword, WriteKeyEncrypted)).

publish_invalid_semver(Config) ->
    App = mock_app(Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = State = rebar_state:new(),
    Exp = {error, {rebar3_hex_publish,
                   {invalid_semver,<<"bad_ver">>,"0.a.1b..0.-foo"}}},
    Exp = rebar3_hex_publish:publish(App, Repo, State),
    ok.

mock_app(Config) ->
    Src = filename:join([?config(data_dir, Config), "test_apps/bad_ver"]),
    {ok, App} = rebar_app_info:discover(Src),
    App.
