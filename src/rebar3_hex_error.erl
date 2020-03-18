-module(rebar3_hex_error).

-export([format_error/1]).

format_error({required, repo}) ->
    "A repository argument is required for this command.";
format_error({error, no_read_key}) ->
    "No read key found for user. Be sure to authenticate first with:"
    ++ " rebar3 hex user auth";
format_error({error, no_write_key}) ->
    "No write key found for user. Be sure to authenticate first with:"
    ++ " rebar3 hex user auth";
format_error(Reason) ->
    try io_lib:format("~p", [Reason]) of
        Result ->
            Result
    catch
        _:_  ->
            io_lib:format("Unknown error encountered : ~ts", [Reason])
    end.

