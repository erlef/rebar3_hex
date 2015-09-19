-module(rebar3_hex_pkg).

-export([init/1,
         do/1,
         format_error/1]).

-export([publish/2
        ,publish/6]).

-include("rebar3_hex.hrl").

-define(PROVIDER, publish).
-define(DEPS, [{default, app_discovery}]).

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
                                {example, "rebar3 hex publish"},
                                {short_desc, "Publish a new version of your package and update the package"},
                                {desc, ""},
                                {opts, [{revert, undefined, "revert", string, "Revert given version."}]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar3_hex_utils:select_apps(rebar_state:project_apps(State)),
    lists:foldl(fun(App, {ok, StateAcc}) ->
                        do_(App, StateAcc)
                end, {ok, State}, Apps).

-spec format_error(any()) -> iolist().
format_error(undefined_server_error) ->
    "Unknown server error";
format_error(Error) ->
    Message = proplists:get_value(<<"message">>, Error, ""),
    Errors = proplists:get_value(<<"errors">>, Error, ""),
    ErrorString = errors_to_string(Errors),

    io_lib:format("Hex Error: ~s~n\t~s", [Message, ErrorString]).

%% ===================================================================
%% Public API
%% ===================================================================

do_(App, State) ->
    Name = rebar_app_info:name(App),

    {Args, _} = rebar_state:command_parsed_args(State),
    Revert = proplists:get_value(revert, Args, undefined),
    case Revert of
        undefined ->
            publish(App, State);
        Version ->
            case delete(Name, Version) of
                ok ->
                    {ok, State};
                Error ->
                    Error
            end
    end.

publish(App, State) ->
    AppDir = rebar_app_info:dir(App),
    Name = rebar_app_info:name(App),

    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(Version,
                                          rebar_app_info:dir(App),
                                          rebar_state:resources(State)),
    {application, _, AppDetails} = rebar3_hex_utils:update_app_src(App, ResolvedVersion),

    Deps = rebar_state:get(State, {locks, default}, []),
    TopLevel = [{N, V} || {_,{pkg,N,V},0} <- Deps],
    Excluded = [binary_to_list(N) || {N,{T,_,_},0} <- Deps, T =/= pkg],

    case publish(AppDir, Name, ResolvedVersion, TopLevel, Excluded, AppDetails) of
        ok ->
            {ok, State};
        Error ->
            Error
    end.

publish(AppDir, Name, Version, Deps, Excluded, AppDetails) ->
    Description = list_to_binary(proplists:get_value(description, AppDetails, "")),
    FilePaths = proplists:get_value(files, AppDetails, ?DEFAULT_FILES),
    AppSrc = [{application, ec_cnv:to_atom(Name), AppDetails}],
    Files = rebar3_hex_utils:expand_paths(FilePaths, AppDir),

    AppFileSrc = filename:join("src", ec_cnv:to_list(Name)++".app.src"),
    AppSrcBinary = ec_cnv:to_binary(lists:flatten(io_lib:format("~p.\n", [AppSrc]))),
    Files1 = [{AppFileSrc, AppSrcBinary} | lists:delete(AppFileSrc, Files)],

    Contributors = proplists:get_value(contributors, AppDetails, []),
    Licenses = proplists:get_value(licenses, AppDetails, []),
    Links = proplists:get_value(links, AppDetails, []),

    Optional = [{app, Name}
               ,{requirements, Deps}
               ,{contributors, Contributors}
               ,{precompiled, false}
               ,{parameters, []}
               ,{description, Description}
               ,{files, Files1}
               ,{licenses, Licenses}
               ,{links, Links}
               ,{build_tools, [<<"rebar3">>]}],
    OptionalFiltered = [{Key, Value} || {Key, Value} <- Optional, Value =/= []],
    Meta = [{name, Name}, {version, Version} | OptionalFiltered],

    {ok, Auth} = rebar3_hex_config:auth(),
    ec_talk:say("Publishing ~s ~s", [Name, Version]),
    ec_talk:say("  Dependencies:~n    ~s", [format_deps(Deps)]),
    ec_talk:say("  Excluded dependencies (not part of the Hex package):~n    ~s", [string:join(Excluded, "\n    ")]),
    ec_talk:say("  Included files:~n    ~s", [string:join(Files, "\n    ")]),
    case ec_talk:ask_default("Proceed?", boolean, "Y") of
        true ->
            upload_package(Auth, Name, Version, Meta, Files1);
        _ ->
            ec_talk:say("Goodbye..."),
            ok
    end.

%% Internal functions

delete(Name, Version) ->
    {ok, Auth} = rebar3_hex_config:auth(),
    case rebar3_hex_http:delete(filename:join([?ENDPOINT, Name, "releases", Version]), Auth) of
        ok ->
            rebar_api:info("Successfully deleted package ~s ~s", [Name, Version]),
            case ec_talk:ask_default(io_lib:format("Also delete tag v~s?", [Version]), boolean, "N") of
                true ->
                    rebar_utils:sh(io_lib:format("git tag -d v~s", [Version]), []);
                _ ->
                    ok
            end;
        {error, _} ->
            rebar_api:error("Unable to delete package ~s ~s", [Name, Version])
    end.

upload_package(Auth, Name, Version, Meta, Files) ->
    {ok, Tar} = rebar3_hex_tar:create(Name, Version, Meta, Files),
    case rebar3_hex_http:post(filename:join([?ENDPOINT, Name, "releases"])
                             ,Auth
                             ,Tar
                             ,integer_to_list(byte_size(Tar))) of
        ok ->
            rebar_api:info("Published ~s ~s", [Name, Version]),
            ok;
        {error, _, Error} ->
            ?PRV_ERROR(Error)
    end.

errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string({Key, Value}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Value)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Values)]) || Values <- Errors]).

format_deps(Deps) ->
    string:join([binary_to_list(<<N/binary, " ", V/binary>>) || {N, V} <- Deps], "\n    ").
