-module(rebar3_hex_owner).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, owner).
-define(DEPS, []).

-define(ENDPOINT, "packages").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex owner"},
                                {short_desc, "Add, remove or list package owners"},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["add", Package, Email] ->
            add(Package, Email),
            {ok, State};
        ["remove", Package, Email] ->
            remove(Package, Email),
            {ok, State};
        ["list", Package] ->
            list(Package),
            {ok, State};
        Command ->
            ?PRV_ERROR({bad_command, Command})
    end.

-spec format_error(any()) -> iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).

add(Package, Email) ->
    {ok, Auth} = rebar3_hex_config:auth(),
    Path = filename:join([?ENDPOINT, Package, "owners", rebar3_hex_http:encode(Email)]),
    case rebar3_hex_http:put(Path, [Auth], []) of
        ok ->
            ec_talk:say("Added ~s to ~s", [Email, Package]);
        {error, Status, Error} ->
            rebar_api:error("Unable to remove package ~p, ~p (~p)", [Package, Error, Status])
    end.

remove(Package, Email) ->
    {ok, Auth} = rebar3_hex_config:auth(),
    Path = filename:join([?ENDPOINT, Package, "owners", rebar3_hex_http:encode(Email)]),
    case rebar3_hex_http:delete(Path, [Auth]) of
        ok ->
            ec_talk:say("Removed ~s from ~s", [Email, Package]);
        {error, Status} ->
            rebar_api:error("Unable to remove package ~p (~p)", [Package, Status])
    end.

list(Package) ->
    {ok, Auth} = rebar3_hex_config:auth(),
    case rebar3_hex_http:get(filename:join([?ENDPOINT, Package, "owners"]), [Auth]) of
        {ok, Map} ->
            Owners = [binary_to_list(maps:get(<<"email">>, Owner, <<"">>)) || Owner <- Map],
            OwnersString = string:join(Owners, "\n"),
            ec_talk:say("~s", [OwnersString]);
        {error, 404} ->
            rebar_api:error("No package with name ~s", [Package]);
        _ ->
            rebar_api:error("Failed to retrieve package information", [])
    end.
