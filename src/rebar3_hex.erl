-module(rebar3_hex).

-export([init/1]).

init(State) ->
    rebar3_hex_user:init(State).
