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
    {Args, _} = rebar_state:command_parsed_args(State),
    Term = proplists:get_value(term, Args, ""),
    {ok, Parents} = rebar3_hex_utils:parent_repos(State),
    lists:foreach(fun(Repo) -> search(State, Repo, Term) end, Parents),
    {ok, State}.

search(State, Repo, Term) ->
    HexConfig = rebar3_hex_utils:hex_config_read(Repo),
    case hex_api_package:search(HexConfig, rebar_utils:to_binary(Term), []) of
        {ok, {200, _Headers, []}} ->
            io:format("No Results~n"),
            {ok, State};
        {ok, {200, _Headers, Packages}} ->
            Header = ["Name", "Version", "Description", "URL"],
            Rows = lists:map(fun(Package) ->
                                     #{<<"name">> := Name,
                                       <<"meta">> := #{<<"description">> := Description},
                                       <<"releases">> := Releases,
                                       <<"html_url">> := Url
                                      } = Package,

                                     Descrip = truncate_description(Description),
                                     [binary_to_list(Name),
                                      latest_stable(Releases), Descrip, unicode:characters_to_list(Url)]

                             end, sort_by_downloads(Packages)),
            ok = print_table([Header] ++ Rows),
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({status, Status});
        {error, Reason} ->
            ?PRV_ERROR({error, Reason})
    end.


truncate_description(Description) ->
    Descrip = string:sub_string(
                string:strip(
                  string:strip(
                    unicode:characters_to_list(Description), both, $\n)
                 ), 1, 50),
    Blist = binary:split(unicode:characters_to_binary(Descrip), <<"\n">>, [global]),
    Slist = lists:map(fun(B) -> unicode:characters_to_list(B) end, Blist),
    Dstr = string:join(Slist, ""),
    case size(Description) of
        N when N >= 50 ->
            Dstr ++ "...";
        _ ->
            Dstr
    end.

sort_by_downloads(Packages) ->
    {Unused, Popular} = lists:partition(fun(P) -> maps:get(<<"downloads">>, P) == #{} end, Packages),
    lists:sort(fun(#{<<"downloads">> := #{<<"all">> := A}},
                   #{<<"downloads">> := #{<<"all">> := B}}) ->
                       A > B
               end,
               Popular) ++ Unused.

latest_stable(Releases) ->
    case gather_stable_releases(Releases) of
        [] ->
            "";
        [Latest | _Rest] ->
            binary_to_list(maps:get(<<"version">>, Latest))
    end.

gather_stable_releases(Releases) ->
    version_sort(lists:filter(fun(#{<<"version">> := Ver}) ->
                            {ok, V} = verl:parse(Ver),
                            case V of
                             #{pre := []} ->
                                 true;
                             _ ->
                                 false
                         end
                 end,
                 Releases
                )).

version_sort(Releases) ->
    lists:sort(fun(#{<<"version">> := A}, #{<<"version">> := B}) ->
                       At = list_to_tuple(binary:split(A, <<".">>, [global])),
                       Bt = list_to_tuple(binary:split(B, <<".">>, [global])),
                       At >= Bt
               end,
               Releases).

-spec format_error(any()) -> iolist().
format_error({status, Status}) ->
    io_lib:format("Error searching for packages: ~ts",
                  [rebar3_hex_utils:pretty_print_status(Status)]);
format_error({error, Reason}) ->
    io_lib:format("Error searching for packages: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

print_table(Rows) ->
    Table = table(Rows),
    io:fwrite(Table),
    ok.

underline_emphasis(Item) ->
    io_lib:format("\e[1m\e[00m\e[4m~ts\e[24m", [Item]).

% Returns a str, expects first row to be a header
table(Rows) ->
    [Header | Body] = align_rows(Rows),
    Table = [pretty_header(Header), ""] ++ Body,
    lists:foldl(fun(Row, Acc) ->
                        Acc ++ [io_lib:fwrite("~s~n", [lists:flatten(Row)])]
                end,
                [],
                Table).

pretty_header(Header) ->
    lists:map(fun(W) ->
                      [Value, Space] = rebar3_hex_utils:str_split(W, " "),
                      underline_emphasis(Value) ++ " "  ++ Space  end,
              Header).

align_rows(Rows) ->
    WidestCells = widest_cells(Rows),
    [align_cells(R, WidestCells) || R <- Rows].

align_cells(Row, WidestCells) ->
    Padded = rpad_row(Row, length(WidestCells), ""),
    [ string:left(Cell, Length + 2, $\s)
      || {Cell, Length} <- lists:zip(Padded, WidestCells)].

widest_cells(Rows) ->
    lists:foldl( fun(Row, Acc) ->
                         CellLengths = [length(C) || C <- Row ],
                         Widest = lists:max([length(Acc), length(CellLengths)]),
                         Padded = rpad_row(CellLengths, Widest, 0),
                         WidestPadded = rpad_row(Acc, Widest, 0),
                         [ lists:max([A, B]) || {A, B} <- lists:zip(Padded, WidestPadded)]
                 end,
                 [],
                 Rows).

rpad_row(L, Length, Elem) ->
    L ++ lists:duplicate(Length - length(L), Elem).
