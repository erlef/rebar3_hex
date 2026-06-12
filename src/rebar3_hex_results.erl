%% @private
-module(rebar3_hex_results).

-export([errors_to_string/1, print_table/1, print_table/2, colorize/2]).

-include("rebar3_hex.hrl").

%% ANSI color codes
-define(COLOR_RED,    "\e[31m").
-define(COLOR_GREEN,  "\e[32m").
-define(COLOR_YELLOW, "\e[33m").
-define(COLOR_RESET,  "\e[0m").

errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string(Map) when is_map(Map) ->
    errors_to_string(maps:to_list(Map));
errors_to_string({<<"inserted_at">>, E}) ->
    lists:flatten(io_lib:format("Inserted At: ~s~n", [E]));
errors_to_string({<<"requirements">>,  Rs}) ->
    lists:flatten([
        "Requirements could not be computed\n",
        [io_lib:format("~s\n~20.20c\n~s\n",[P,$-, R]) || {P, R} <- maps:to_list(Rs)]
    ]);
errors_to_string({Key, Val}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Val)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Vals)]) || Vals <- Errors]).

print_table(Rows) ->
    Table = table(Rows),
    io:fwrite(Table),
    ok.

%% Like print_table/1, but applies a per-column colorizer function to each
%% body row's cell. `Colorizers' is a map of column index (1-based) to a
%% function taking the row and returning a color code (or `undefined' for
%% no color). Colorizing is applied after column alignment so it doesn't
%% affect padding.
print_table(Rows, Colorizers) ->
    Table = table(Rows, Colorizers),
    io:fwrite(Table),
    ok.

%% Wraps the visible (non-padding) portion of an aligned cell with the given
%% ANSI color code. `undefined' leaves the cell unchanged.
colorize(Cell, undefined) ->
    Cell;
colorize(Cell, Color) ->
    Trimmed = string:trim(Cell, trailing),
    Padding = string:slice(Cell, string:length(Trimmed)),
    color_code(Color) ++ Trimmed ++ ?COLOR_RESET ++ Padding.

color_code(red)    -> ?COLOR_RED;
color_code(green)  -> ?COLOR_GREEN;
color_code(yellow) -> ?COLOR_YELLOW.

underline_emphasis(Item) ->
    io_lib:format("\e[1m\e[00m\e[4m~ts\e[24m", [Item]).

% Returns a str, expects first row to be a header
table(Rows) ->
    table(Rows, #{}).

% Like table/1, but applies per-column colorizers (see print_table/2) to
% the body rows.
table([Headers | Body], Colorizers) ->
    Headers1 = [H ++ "|" || H <- Headers],
    [Headers2 | Body1] = align_rows([Headers1 | Body]),
    Body2 = colorize_rows(Body, Body1, Colorizers),
    Table = [pretty_header(Headers2), ""] ++ Body2,
    lists:foldl(fun(Row, Acc) ->
                    Acc ++ [io_lib:fwrite("~s~n", [lists:flatten(Row)])]
                end,
                [],
                Table).

colorize_rows(_Rows, AlignedRows, Colorizers) when map_size(Colorizers) =:= 0 ->
    AlignedRows;
colorize_rows(Rows, AlignedRows, Colorizers) ->
    [colorize_row(Row, AlignedRow, Colorizers)
        || {Row, AlignedRow} <- lists:zip(Rows, AlignedRows)].

colorize_row(Row, AlignedRow, Colorizers) ->
    [colorize_cell(Row, Cell, Idx, Colorizers)
        || {Cell, Idx} <- lists:zip(AlignedRow, lists:seq(1, length(AlignedRow)))].

colorize_cell(Row, Cell, Idx, Colorizers) ->
    case maps:find(Idx, Colorizers) of
        {ok, ColorFun} ->
            colorize(Cell, ColorFun(Row));
        error ->
            Cell
    end.

pretty_header(Header) ->
    lists:map(fun(W)  ->
                [Value, Space] = rebar3_hex_io:str_split(W, "|"),
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
    lists:foldl(fun(Row, Acc) ->
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
