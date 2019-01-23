-module(rebar3_hex_key).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, key).
-define(DEPS, []).

-define(ENDPOINT, "keys").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex key [list | remove <key>]"},
                                 {short_desc, "Remove or list API keys associated with your account"},
                                 {desc, ""},
                                 {opts, []}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

% TODO: Adjust the spec when this is implemented
%-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
-spec do(rebar_state:t()) -> {'error',{'rebar3_hex_key','bad_command' | 'not_implemented'}}.
do(State) ->
    case rebar_state:command_args(State) of
        ["remove", _KeyName] ->
            ?PRV_ERROR(not_implemented);
        ["list"] ->
            ?PRV_ERROR(not_implemented);
        _ ->
            ?PRV_ERROR(bad_command)
    end.

-spec format_error(any()) -> iolist().
format_error(not_implemented) ->
    "Not implemented";
format_error(bad_command) ->
    "Unknown command. Command must be remove or list.".
