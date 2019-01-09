-module(rebar3_hex_user_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [encrypt_decrypt_write_key].

encrypt_decrypt_write_key(_Config) ->
    WriteKey = <<"abc1234">>,

    Username = <<"user">>,
    LocalPassword = <<"password">>,

    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    ?assertThrow({error,{rebar3_hex_user,bad_local_password}},
                 rebar3_hex_user:decrypt_write_key(Username, <<"wrong password">>, WriteKeyEncrypted)),

    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, LocalPassword, WriteKeyEncrypted)).
