-module(rebar3_hex_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [encrypt_decrypt_write_key].

encrypt_decrypt_write_key(_Config) ->
    WriteKey = <<"abc1234">>,

    Username = <<"user">>,
    LocalPassword = <<"password">>,

    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, LocalPassword, WriteKeyEncrypted)).
