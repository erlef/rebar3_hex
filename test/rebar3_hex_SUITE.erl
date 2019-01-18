-module(rebar3_hex_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [init_test].

init_test(_Config) ->
    {ok, State} = rebar3_hex:init(rebar_state:new()),
    ?assertEqual(state_t, element(1, State)).
