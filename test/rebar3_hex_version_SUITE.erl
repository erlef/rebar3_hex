-module(rebar3_hex_version_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        parse_test,
        increment_test,
        format_test
    ].

parse_test(_Config) ->
    Exp = {ok, #{build => undefined, major => 1, minor => 1, patch => 1, pre => []}},
    ?assertMatch(Exp, rebar3_hex_version:parse("1.1.1")),
    ?assertMatch(Exp, rebar3_hex_version:parse(<<"1.1.1">>)),

    ExpErr = {error, invalid_version},
    ?assertMatch(ExpErr, rebar3_hex_version:parse("1.1")),
    ?assertMatch(ExpErr, rebar3_hex_version:parse(<<"1.1">>)).

increment_test(_Config) ->
    Ver = #{major => 1, minor => 1, patch => 1, pre => [], build => undefined},
    Exp1 = #{major => 2, minor => 0, patch => 0, pre => [], build => undefined},
    ?assertMatch(Exp1, rebar3_hex_version:increment(major, Ver)),

    Exp2 = #{major => 1, minor => 2, patch => 0, build => undefined, pre => []},
    ?assertMatch(Exp2, rebar3_hex_version:increment(minor, Ver)),

    Exp3 = #{major => 1, minor => 1, patch => 2, build => undefined, pre => []},
    ?assertMatch(Exp3, rebar3_hex_version:increment(patch, Ver)).

format_test(_Config) ->
    Ver = #{build => undefined, major => 1, minor => 1, patch => 1, pre => []},
    Exp = <<"1.1.1">>,
    ?assertMatch(Exp, rebar3_hex_version:format(Ver)).
