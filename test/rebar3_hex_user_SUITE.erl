-module(rebar3_hex_user_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [init_test, encrypt_decrypt_write_key].

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

encrypt_decrypt_write_key(_Config) ->
    WriteKey = <<"abc1234">>,

    Username = <<"user">>,
    LocalPassword = <<"password">>,

    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    ?assertThrow({error,{rebar3_hex_user,bad_local_password}},
                  rebar3_hex_user:decrypt_write_key(Username, <<"wrong password">>, WriteKeyEncrypted)),

    ?assertEqual(WriteKey, rebar3_hex_user:decrypt_write_key(Username, LocalPassword, WriteKeyEncrypted)).
