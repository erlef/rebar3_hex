-module(rebar3_hex_utils).

-export([hex_home/0
        ,binarify/1]).

-define(DEFAULT_HEX_DIR, ".hex").

hex_home() ->
    {ok, [[Home]]} = init:get_argument(home),
    filename:join(Home, ?DEFAULT_HEX_DIR).

binarify(Term) when is_boolean(Term) ->
    Term;
binarify(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
binarify([]) ->
    [];
binarify(Term) when is_list(Term) ->
    case io_lib:printable_list(Term) of
        true ->
            list_to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.
