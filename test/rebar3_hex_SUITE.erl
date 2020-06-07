-module(rebar3_hex_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [task_args_test, gather_opts_test, init_test, help_test, repo_opt].


gather_opts_test(_Config) -> 
    State = rebar_state:new(),
    CmdArgs = {[{foo,"bar"}, {count, 42}, {other, eh}], []},
    State1 = rebar_state:command_parsed_args(State, CmdArgs),
    ?assertMatch(#{count := 42, foo := "bar"}, rebar3_hex:gather_opts([count, foo], State1)).

task_args_test(_Config) -> 
    State = rebar_state:new(),
    CmdArgs = {[{task, thing} | {[{foo,"bar"}, {count, 42}], []}], []},
    State1 = rebar_state:command_parsed_args(State, CmdArgs),
    ?assertMatch({thing,{[{foo,"bar"},{count,42}],[]}}, rebar3_hex:task_args(State1)).

repo_opt(_Config) ->
    ?assertEqual({repo,114,"repo",string,
      "Repository to use for this command."}, rebar3_hex:repo_opt()).

init_test(_Config) ->
    {ok, State} = rebar3_hex:init(rebar_state:new()),
    ?assertEqual(state_t, element(1, State)).

%% Smoke test to ensure we don't crash
help_test(_Config) -> 
    %% Silent output during our tests
    ok =  meck:new(io_lib, [unstick, passthrough]),
    meck:expect(io_lib, format, 2, fun(_,_) -> "" end),
    Checks = [
                {rebar3_hex_publish, publish}, 
                {rebar3_hex_revert, revert},
                {rebar3_hex_cut, cut},
                {rebar3_hex_key, key},
                {rebar3_hex_owner, owner},
                {rebar3_hex_repo, repo},
                {rebar3_hex_retire, retire},
                {rebar3_hex_search, search},
                {rebar3_hex_user, user}
               ],
    lists:foreach(fun get_help/1, Checks),
    meck:unload(io_lib),
    ok.

get_help({Mod, Task}) -> 
    State = rebar_state:new(),
    {ok, State1} = Mod:init(State),
    Providers = rebar_state:providers(State1),
    Provider = providers:get_provider(Task, Providers, hex),
    ?assertMatch(ok, providers:help(Provider)).

