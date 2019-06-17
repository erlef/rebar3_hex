-module(rebar3_hex_owner).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, owner).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex owner"},
                                 {short_desc, "Add, remove or list package owners"},
                                 {desc, ""},
                                 {opts, [rebar3_hex_utils:repo_opt()]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar3_hex_utils:repo(State) of
        {ok, Repo} ->
            handle_command(State, Repo);
        {error, Reason} ->
            ?PRV_ERROR(Reason)
    end.

handle_command(State, Repo) ->
    case command_args(State) of
        {"add", Package, UsernameOrEmail} ->
            {ok, Config} = rebar3_hex_utils:hex_config_write(Repo),
            add(Config, Package, UsernameOrEmail, State),
            {ok, State};
        {"remove", Package, UsernameOrEmail} ->
            {ok, Config} = rebar3_hex_utils:hex_config_write(Repo),
            remove(Config, Package, UsernameOrEmail, State),
            {ok, State};
        {"list", Package} ->
            {ok, Config} = rebar3_hex_utils:hex_config_read(Repo),
            list(Config, Package, State),
            {ok, State};
        _Command ->
            ?PRV_ERROR(bad_command)
    end.

command_args(State) ->
    case get_args(rebar_state:command_args(State)) of
        {"list", Package} ->
            {"list", rebar_utils:to_binary(Package)};
        {Command, Package, UserOrEmail} ->
            {Command, rebar_utils:to_binary(Package), rebar_utils:to_binary(UserOrEmail)};
        BadCommand ->
          BadCommand
     end.

get_args(["list", Package]) ->
    {"list", Package};
get_args([Task, Package, UserName]) when Task =:= "add" orelse Task =:= "remove" ->
    {Task, Package, UserName};
get_args([Task, Package, UserName, "-r", _]) ->
    {Task, Package, UserName};
get_args(BadCommand) ->
    BadCommand.

-spec format_error(any()) -> iolist().
format_error(bad_command) ->
    "Command must be one of add, remove or list";
format_error({error, Package, Reason}) ->
    io_lib:format("Error listing owners of package ~ts: ~p", [Package, Reason]);
format_error({status, Status, Package}) ->
    io_lib:format("Error listing owners of package ~ts: ~ts",
                  [Package, rebar3_hex_utils:pretty_print_status(Status)]);
format_error({error, Package, UsernameOrEmail, Reason}) ->
    io_lib:format("Error adding ~ts as owner of package ~ts: ~p", [UsernameOrEmail, Package, Reason]);
format_error({status, Status, Package, UsernameOrEmail}) ->
    io_lib:format("Error adding ~ts as owner of package ~ts: ~ts",
                  [UsernameOrEmail, Package, rebar3_hex_utils:pretty_print_status(Status)]).

add(HexConfig, Package, UsernameOrEmail, State) ->
    case hex_api_package_owner:add(HexConfig, Package, UsernameOrEmail) of
        {ok, {204, _Headers, _Body}} ->
            ec_talk:say("Added ~ts to ~ts", [UsernameOrEmail, Package]),
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({Status, Package, UsernameOrEmail});
        {error, Reason} ->
            ?PRV_ERROR({error, Package, UsernameOrEmail, Reason})
    end.

remove(HexConfig, Package, UsernameOrEmail, State) ->
    case hex_api_package_owner:delete(HexConfig, Package, UsernameOrEmail) of
        {ok, {204, _Headers, _Body}} ->
            ec_talk:say("Removed ~ts from ~ts", [UsernameOrEmail, Package]),
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({status, Status, Package, UsernameOrEmail});
        {error, Reason} ->
            ?PRV_ERROR({error, Package, UsernameOrEmail, Reason})
    end.

list(HexConfig, Package, State) ->
    case hex_api_package_owner:list(HexConfig, Package) of
        {ok, {200, _Headers, List}} ->
            Owners = [binary_to_list(maps:get(<<"email">>, Owner, <<"">>)) || Owner <- List],
            OwnersString = rebar_string:join(Owners, "\n"),
            ec_talk:say("~s", [OwnersString]),
            {ok, State};
        {ok, {Status, _Headers, _Body}} ->
            ?PRV_ERROR({status, Status, Package});
        {error, Reason} ->
            ?PRV_ERROR({error, Package, Reason})
    end.
