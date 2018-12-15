-module(rebar3_hex_docs).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, docs).
-define(DEPS, [{default, edoc}]).

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
                                {example, "rebar3 hex docs"},
                                {short_desc, "Publish documentation for the current project and version"},
                                {desc, ""},
                                {opts, [{revert, undefined, "revert", string, "Revert given version."},
                                        rebar3_hex_utils:repo_opt()]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar3_hex_utils:select_apps(rebar_state:project_apps(State)),
    Repo = rebar3_hex_utils:repo(State),
    case maps:get(write_key, Repo, undefined) of
        undefined ->
            ?PRV_ERROR(no_write_key);
        _ ->
            lists:foldl(fun(App, {ok, StateAcc}) ->
                                do_(App, StateAcc)
                        end, {ok, State}, Apps)
    end.

-spec format_error(any()) -> iolist().
format_error({publish, Status, Package, Version}) when is_integer(Status) ->
    io_lib:format("Error publishing docs for package ~ts ~ts: ~ts",
                  [Package, Version, rebar3_hex_utils:pretty_print_status(Status)]);
format_error({publish, Package, Version, Reason}) ->
    io_lib:format("Error publishing docs for package ~ts ~ts: ~p", [Package, Version, Reason]);
format_error({revert, Status, Package, Version}) when is_integer(Status) ->
    io_lib:format("Error deleting docs for package ~ts ~ts: ~ts",
                  [Package, Version, rebar3_hex_utils:pretty_print_status(Status)]);
format_error({revert, Package, Version, Reason}) ->
    io_lib:format("Error deleting docs for package ~ts ~ts: ~p", [Package, Version, Reason]).

do_(App, State) ->
    AppDir = rebar_app_info:dir(App),
    Files = rebar3_hex_utils:expand_paths(["doc"], AppDir),
    AppDetails = rebar_app_info:app_details(App),
    Name = binary_to_list(rebar_app_info:name(App)),
    PkgName = ec_cnv:to_list(proplists:get_value(pkg_name, AppDetails, Name)),
    {Args, _} = rebar_state:command_parsed_args(State),
    Revert = proplists:get_value(revert, Args, undefined),

    Repo = rebar3_hex_utils:repo(State),

    case Revert of
        undefined ->
            Vsn = rebar_app_info:original_vsn(App),

            Tarball = PkgName++"-"++Vsn++"-docs.tar.gz",
            ok = erl_tar:create(Tarball, file_list(Files), [compressed]),
            {ok, Tar} = file:read_file(Tarball),
            file:delete(Tarball),

            case hex_api_publish_docs(Repo, rebar_utils:to_binary(PkgName), rebar_utils:to_binary(Vsn), Tar) of
                {ok, {201, _Headers, _Body}} ->
                    rebar_api:info("Published docs for ~ts ~ts", [PkgName, Vsn]),
                    {ok, State};
                {ok, {Status, _Headers, _Body}} ->
                    ?PRV_ERROR({publish, Status, PkgName, Vsn});
                {error, Reason} ->
                    ?PRV_ERROR({publish, PkgName, Vsn, Reason})
            end;
        Vsn ->
            case hex_api_delete_docs(Repo, rebar_utils:to_binary(PkgName), rebar_utils:to_binary(Vsn)) of
                {ok, {204, _Headers, _Body}} ->
                    rebar_api:info("Successfully deleted docs for ~ts ~ts", [Name, Vsn]),
                    ok;
                {ok, {Status, _Headers, _Body}} ->
                    ?PRV_ERROR({revert, Status, PkgName, Vsn});
                {error, Reason} ->
                    ?PRV_ERROR({revert, PkgName, Vsn, Reason})
            end
    end.

hex_api_publish_docs(Config0, Name, Version, Tarball) ->
    Username = maps:get(username, Config0),
    WriteKey = maps:get(write_key, Config0),
    Config = Config0#{api_key => rebar3_hex_user:decrypt_write_key(Username, WriteKey)},

    TarballContentType = "application/octet-stream",

    Headers = maps:get(http_headers, Config, #{}),
    Headers1 = maps:put(<<"content-length">>, integer_to_binary(byte_size(Tarball)), Headers),
    Config2 = maps:put(http_headers, Headers1, Config),

    Body = {TarballContentType, Tarball},
    hex_api:post(Config2, ["packages", Name, "releases", Version, "docs"], Body).

hex_api_delete_docs(Config, Name, Version) ->
    hex_api:delete(Config, ["packages", Name, "releases", Version, "docs"]).

file_list(Files) ->
    [{drop_path(ShortName, ["doc"]), FullName} || {ShortName, FullName} <- Files].

drop_path(File, Path) ->
    filename:join(filename:split(File) -- Path).
