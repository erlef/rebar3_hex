-module(rebar3_hex_search).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("rebar3_hex.hrl").

-define(PROVIDER, search).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex search <term>"},
                                {short_desc, "Display packages matching the given search query"},
                                {desc, ""},
                                {opts, [{term, undefined, undefined, string, "Search term."}]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_packages:packages(State),
    {Args, _} = rebar_state:command_parsed_args(State),
    Term = proplists:get_value(term, Args, undefined),
    ets:foldl(fun({Name, Vsns}, ok) when is_binary(Name) ->
                      case string:str(binary_to_list(Name), Term) of
                          0 ->
                              ok;
                          N when N >= 0 ->
                              Vsns1 = [binary_to_list(Vsn) || Vsn <- Vsns],
                              Vsns2 = string:join(Vsns1, ", "),
                              io:format("~s: ~s~n", [Name, Vsns2])
                      end;
                 (_, ok) ->
                      ok
              end, ok, package_index),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
