-module(rebar3_hex).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_hex_user:init(State),
    {ok, State2} = rebar3_hex_config:init(State1),
    {ok, State3} = rebar3_hex_key:init(State2),
    rebar3_hex_pkg:init(State3).
