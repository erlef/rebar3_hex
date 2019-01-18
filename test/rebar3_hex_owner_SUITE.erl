-module(rebar3_hex_owner_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [format_error_test].

format_error_test(_Config) ->
    Exp1 = "Command must be one of add, remove or list",
    ?assertEqual(Exp1, rebar3_hex_owner:format_error(bad_command)),
    Exp2 = <<"Error listing owners of package foo: because">>,
    ?assertEqual(Exp2, list_to_bitstring(rebar3_hex_owner:format_error({error, foo,
                                                      because}))),
    Exp3 = <<"Error adding user as owner of package foo: because">>,
    ?assertEqual(Exp3, list_to_bitstring(rebar3_hex_owner:format_error({error,
                                                                        foo,
                                                                        user,
                                                      because}))),
    Exp4 = <<"Error adding user as owner of package foo: Entity not found (404)">>,
    ?assertEqual(Exp4, list_to_bitstring(rebar3_hex_owner:format_error({status,
                                                                        404,
                                                                        foo,
                                                                        user}))),
    Exp5 = <<"Error listing owners of package foo: Entity not found (404)">>,
    ?assertEqual(Exp5, list_to_bitstring(rebar3_hex_owner:format_error({status,
                                                                        404,
                                                                        foo}))).
