-module(rebar3_hex_user_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [init_test, encrypt_decrypt_write_key_test, format_error_test].

init_test(_Config) ->
    {ok, State} = rebar3_hex_user:init(rebar_state:new()),
    Exp = [{provider,user,rebar3_hex_user,
                      {[],[]},
                      true,[],[],"Hex user tasks","rebar3 hex user <command>",
                      [{repo,114,"repo",string,
                             "Repository to use for this command."}],
                      [default],
                      hex}],
    ?assertMatch(Exp, rebar_state:providers(State)),
    ?assertError(function_clause, rebar3_hex_user:init({})).

encrypt_decrypt_write_key_test(_Config) ->
    WriteKey = <<"abc1234">>,

    Username = <<"user">>,
    LocalPassword = <<"password">>,

    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    ?assertThrow({error,{rebar3_hex_user,bad_local_password}},
                  rebar3_hex_user:decrypt_write_key(Username, <<"wrong password">>, WriteKeyEncrypted)),

    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, LocalPassword, WriteKeyEncrypted)).

format_error_test(_Config) ->
    ?assertEqual(<<"Fetching currently authenticated user failed: eh">>,
                 list_to_bitstring(rebar3_hex_user:format_error({whoami_failure, eh}))),
    ?assertEqual(<<"Failure to decrypt write key: bad local password">>,
                 list_to_bitstring(rebar3_hex_user:format_error(bad_local_password))),
    ?assertEqual(<<"Registration of user failed: eh">>,
                 list_to_bitstring(rebar3_hex_user:format_error({registration_failure, eh}))),
    ?assertEqual(<<"Failure generating authentication tokens: eh">>,
                 list_to_bitstring(rebar3_hex_user:format_error({generate_key, eh}))),
    ?assertEqual(<<"Password confirmation failed. The passwords must match.">>,
                 list_to_bitstring(rebar3_hex_user:format_error(no_match_local_password))),
    ?assertEqual(<<"Command must be one of register, whoami, auth, deauth or reset_password">>,
                 list_to_bitstring(rebar3_hex_user:format_error(bad_command))),
    ?assertEqual(["'eh?'"], rebar3_hex_user:format_error('eh?')).




