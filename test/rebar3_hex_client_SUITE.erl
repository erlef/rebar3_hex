-module(rebar3_hex_client_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [pretty_print_status,
     pretty_print_errors].

pretty_print_errors(_Config) ->
    Errors = #{<<"emails">>=>#{<<"email">>=><<"already in use">>}},
    ?assertEqual(<<"email already in use">>, rebar3_hex_client:pretty_print_errors(Errors)).

pretty_print_status(_Config) ->
    ?assertEqual("Authentication failed (401)", rebar3_hex_client:pretty_print_status(401)),
    ?assertEqual("Forbidden (403)", rebar3_hex_client:pretty_print_status(403)),
    ?assertEqual("Entity not found (404)",
                 rebar3_hex_client:pretty_print_status(404)),
    ?assertEqual("Validation failed (422)",
                 rebar3_hex_client:pretty_print_status(422)),
    ?assertEqual(lists:append("HTTP status code: ", ["418"]),
                 rebar3_hex_client:pretty_print_status(418)).
