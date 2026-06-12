-module(rebar3_hex_outdated_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [format_error_test].

format_error_test(_Config) ->
    Exp1 = <<"Error fetching package information: Entity not found (404)">>,
    Subject1 = list_to_bitstring(rebar3_hex_outdated:format_error({status, 404})),
    ?assertEqual(Exp1, Subject1),

    Exp2 = <<"Error fetching package information: because">>,
    Subject2 = list_to_bitstring(rebar3_hex_outdated:format_error({error, because})),
    ?assertEqual(Exp2, Subject2),

    Exp3 = "A repository argument is required for this command.",
    Subject3 = rebar3_hex_outdated:format_error({required, repo}),
    ?assertEqual(Exp3, Subject3).
