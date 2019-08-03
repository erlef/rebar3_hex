-module(rebar3_hex_docs_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [format_error_test].

format_error_test(_Config) ->
    Exp1 = "Invalid command and/or options provided",
    ?assertEqual(Exp1, rebar3_hex_docs:format_error(bad_command)),

    Exp2 = "Error publishing : Not authorized",
    ?assertEqual(Exp2, rebar3_hex_docs:format_error({publish, {unauthorized, <<>>}})),
    Exp3 = "Error publishing : Package or Package Version not found",
    ?assertEqual(Exp3, rebar3_hex_docs:format_error({publish, {not_found, <<>>}})),

    Exp4 = "Error reverting docs : Not authorized",
    ?assertEqual(Exp4, rebar3_hex_docs:format_error({revert, {unauthorized, <<>>}})),
    Exp5 = "Error reverting docs : Package or Package Version not found",
    ?assertEqual(Exp5, rebar3_hex_docs:format_error({revert, {not_found, <<>>}})),

    Exp6 = [[60,60,"\"something bad\"",62,62]],
    ?assertEqual(Exp6, rebar3_hex_docs:format_error(<<"something bad">>)).
