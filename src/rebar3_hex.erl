-module(rebar3_hex).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_hex_user:init(State),
    {ok, State2} = rebar3_hex_config:init(State1),
    rebar3_hex_pkg:init(State2).
