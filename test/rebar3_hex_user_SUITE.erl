-module(rebar3_hex_user_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [format_error_test].

encrypt_decrypt_write_key_test(_Config) ->
    WriteKey = <<"abc1234">>,

    Username = <<"user">>,
    LocalPassword = <<"password">>,

    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    ?assertError({error,{rebar3_hex_user,bad_local_password}},
                  rebar3_hex_user:decrypt_write_key(Username, <<"wrong password">>, WriteKeyEncrypted)),

    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, LocalPassword, WriteKeyEncrypted)).

format_error_test(_Config) ->
    ?assertEqual(<<"Not authenticated as any user currently for this repository">>,
                 list_to_bitstring(rebar3_hex_user:format_error(not_authenticated))),
    ?assertEqual(<<"Fetching currently authenticated user failed: eh">>,
                 list_to_bitstring(rebar3_hex_user:format_error({whoami, <<"eh">>}))),
    ?assertEqual(<<"Failure to decrypt write key: bad local password">>,
                 list_to_bitstring(rebar3_hex_user:format_error(bad_local_password))),
    ?assertEqual(<<"Registration of user failed: foo bar">>,
                 list_to_bitstring(rebar3_hex_user:format_error({registration_failure, #{<<"foo">> => <<"bar">>}}))),
    ?assertEqual(<<"Failure generating authentication tokens: eh">>,
                 list_to_bitstring(rebar3_hex_user:format_error({generate_key, <<"eh">>}))),
    ?assertEqual(<<"Password confirmation failed. The passwords must match.">>,
                 list_to_bitstring(rebar3_hex_user:format_error(passwords_do_not_match))),
    ?assertEqual(<<"Local passwords can not exceed 32 characters.">>,
                 list_to_bitstring(rebar3_hex_user:format_error(local_password_too_big))),
    ?assertEqual(<<"Error reseting account password: eh?">>,
                 list_to_bitstring(rebar3_hex_user:format_error({reset_account_password, <<"eh?">>}))),
    ?assertMatch(<<"Invalid arguments, expected one of:", _Rest/binary>>,
                 list_to_bitstring(rebar3_hex_user:format_error(bad_command))),
    ?assertEqual("An unknown error was encountered. Run with DIAGNOSTIC=1 for more details.", rebar3_hex_user:format_error('eh?')).
