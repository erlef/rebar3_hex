-module(rebar3_hex_search).

-export([init/1,
         do/1,
         format_error/1]).

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
                                {opts, [{term, undefined, undefined, string, "Search term."},
                                        rebar3_hex_utils:repo_opt()]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Repo = rebar3_hex_utils:repo(State),

    {Args, _} = rebar_state:command_parsed_args(State),
    Term = proplists:get_value(term, Args, ""),
    case hex_api_package:search(Repo, rebar_utils:to_binary(Term), []) of
        {ok, {200, _Headers, []}} ->
            io:format("No Results", []);
        {ok, {200, _Headers, Packages}} ->
            io:format("Results:~n~n", []),
            Results = [[Name, ": ", Description, "\n"] ||
                          #{<<"name">> := Name,
                            <<"meta">> := #{<<"description">> := Description}} <- Packages],
            io:format("~ts", [Results]),
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({status, Status});
        {error, Reason} ->
            ?PRV_ERROR({error, Reason})
    end.

-spec format_error(any()) -> iolist().
format_error({status, Status}) ->
    io_lib:format("Error searching for packages: ~ts",
                  [rebar3_hex_utils:pretty_print_status(Status)]);
format_error({error, Reason}) ->
    io_lib:format("Error searching for packages: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
