-module(rebar3_hex).

-export([init/1]).

init(State) ->
    {ok, State1} = rebar3_hex_user:init(State),
    rebar3_hex_pkg:init(State1).
