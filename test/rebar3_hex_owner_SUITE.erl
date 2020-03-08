-module(rebar3_hex_owner_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [format_error_test].

format_error_test(_Config) ->
    Exp1 = "Invalid command",
    Subject1 =  string:substr(rebar3_hex_owner:format_error(bad_command), 1, 15),
    ?assertEqual(Exp1, Subject1),
    Exp2 = <<"Error listing owners of package foo: because">>,
    Subject2 = list_to_bitstring(rebar3_hex_owner:format_error({error, foo, because})),
    ?assertEqual(Exp2, Subject2),
    Exp3 = <<"Error adding user as owner of package foo: because">>,
    Subject3 = list_to_bitstring(rebar3_hex_owner:format_error({error, foo, user, because})),
    ?assertEqual(Exp3, Subject3),
    Exp4 = <<"Error adding user as owner of package foo: Entity not found (404)">>,
    Subject4 = list_to_bitstring(rebar3_hex_owner:format_error({status, 404, foo, user})),
    ?assertEqual(Exp4, Subject4),
    Exp5 = <<"Error listing owners of package foo: Entity not found (404)">>,
    Subject5 = list_to_bitstring(rebar3_hex_owner:format_error({status, 404, foo})),
    ?assertEqual(Exp5, Subject5).
