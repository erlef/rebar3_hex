-module(rebar3_hex).

-export([init/1]).

init(State) ->
    inets:start(httpc, [{profile, hex}]),
    rebar3_hex_http:maybe_setup_proxy(),
    lists:foldl(fun provider_init/2, {ok, State}, [rebar3_hex_user
                                                  ,rebar3_hex_cut
                                                  ,rebar3_hex_config
                                                  ,rebar3_hex_key
                                                  ,rebar3_hex_info
                                                  ,rebar3_hex_owner
                                                  ,rebar3_hex_docs
                                                  ,rebar3_hex_search
                                                  ,rebar3_hex_pkg]).

provider_init(Module, {ok, State}) ->
    Module:init(State).
