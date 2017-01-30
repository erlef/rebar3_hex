-module(rebar3_hex_pkg).

-export([init/1,
         do/1,
         format_error/1]).

-export([publish/2
        ,publish/6
        ,validate_app_details/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, publish).
-define(DEPS, [{default, lock}]).

-define(ENDPOINT, "packages").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
format_error({non_hex_deps, Excluded}) ->
    io_lib:format("Can not publish package because the following deps are not available in hex: ~s", [string:join(Excluded, ", ")]);
format_error(has_contributors) ->
    "The contributors field is deprecated, please change to maintainers and rerun.";
format_error(undefined_server_error) ->
    "Unknown server error";
format_error({status, Status}) ->
    rebar3_hex_http:pretty_print_status(Status);
format_error({status, Status, undefined_server_error}) ->
    "Unknown server error: " ++ rebar3_hex_http:pretty_print_status(Status);
format_error({status, Status, Error}) ->
    Message = maps:get(<<"message">>, Error, ""),
    Errors = maps:get(<<"errors">>, Error, ""),
    ErrorString = errors_to_string(Errors),

    io_lib:format("Status Code: ~s~nHex Error: ~s~n\t~s", [rebar3_hex_http:pretty_print_status(Status),
                                                           Message, ErrorString]).

%% ===================================================================
%% Public API
%% ===================================================================

do_(App, State) ->
    Name = rebar_app_info:name(App),

    {Args, _} = rebar_state:command_parsed_args(State),
    Revert = proplists:get_value(revert, Args, undefined),
    case Revert of
        undefined ->
            case publish(App, State) of
                ok ->
                    {ok, State};
                stopped ->
                    {ok, State};
                Error ->
                    Error
            end;
        Version ->
            case delete(App, Name, Version) of
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
    TopLevel = [{N, [{<<"app">>, A}, {<<"optional">>, false}, {<<"requirement">>, V}]} || {A,{pkg,N,V,_},0} <- Deps],
    Excluded = [binary_to_list(N) || {N,{T,_,_},0} <- Deps, T =/= pkg],

    case validate_app_details(AppDetails) of
        ok ->
            publish(AppDir, Name, ResolvedVersion, TopLevel, Excluded, AppDetails);
        Error ->
            Error
    end.

publish(AppDir, Name, Version, Deps, [], AppDetails) ->
    Config = rebar_config:consult(AppDir),
    ConfigDeps = proplists:get_value(deps, Config, []),
    Deps1 = update_versions(ConfigDeps, Deps),

    Description = list_to_binary(proplists:get_value(description, AppDetails, "")),
    FilePaths = proplists:get_value(files, AppDetails, ?DEFAULT_FILES),
    IncludeFilePaths = proplists:get_value(include_files, AppDetails, []),
    ExcludeFilePaths = proplists:get_value(exclude_files, AppDetails, []),
    AppSrc = {application, ec_cnv:to_atom(Name), AppDetails},
    Files = lists:ukeysort(2, rebar3_hex_utils:expand_paths(FilePaths, AppDir)),
    IncludeFiles = lists:ukeysort(2, rebar3_hex_utils:expand_paths(IncludeFilePaths, AppDir)),
    ExcludeFiles = lists:ukeysort(2, rebar3_hex_utils:expand_paths(ExcludeFilePaths, AppDir)),

    Files1 = lists:ukeymerge(2, Files, IncludeFiles),
    Files2 = lists:filter(fun ({_, Path}) -> lists:keymember(Path, 2, ExcludeFiles) =/= true end, Files1),

    AppFileSrc = filename:join("src", ec_cnv:to_list(Name)++".app.src"),
    AppSrcBinary = ec_cnv:to_binary(lists:flatten(io_lib:format("~tp.\n", [AppSrc]))),
    FilesAndApp = [{AppFileSrc, AppSrcBinary} | lists:keydelete(AppFileSrc, 1, Files2)],
    MetaDataFiles = [File || {File, _} <- FilesAndApp],

    Maintainers = proplists:get_value(maintainers, AppDetails, []),
    Licenses = proplists:get_value(licenses, AppDetails, []),
    Links = proplists:get_value(links, AppDetails, []),
    BuildTools = proplists:get_value(build_tools, AppDetails, [<<"rebar3">>]),

    %% We check the app file for the 'pkg' key wich allows us to select
    %% a package name other then the app anem, if it is not set we default
    %% back to the app name.
    PkgName = ec_cnv:to_binary(proplists:get_value(pkg_name, AppDetails, Name)),

    Optional = [{app, Name}
               ,{maintainers, Maintainers}
               ,{precompiled, false}
               ,{parameters, []}
               ,{description, Description}
               ,{files, MetaDataFiles}
               ,{licenses, Licenses}
               ,{links, Links}
               ,{build_tools, BuildTools}],
    OptionalFiltered = [{Key, Value} || {Key, Value} <- Optional, Value =/= []],
    Meta = [{name, PkgName}, {version, Version}, {requirements, maps:from_list(Deps1)} | OptionalFiltered],
    Filenames = [F || {_, F} <- Files2],

    {ok, Auth} = rebar3_hex_config:auth(),
    ec_talk:say("Publishing ~s ~s", [PkgName, Version]),
    ec_talk:say("  Description: ~s", [Description]),
    ec_talk:say("  Dependencies:~n    ~s", [format_deps(Deps1)]),
    ec_talk:say("  Included files:~n    ~s", [string:join(Filenames, "\n    ")]),
    ec_talk:say("  Maintainers:~n    ~s", [format_maintainers(Maintainers)]),
    ec_talk:say("  Licenses: ~s", [format_licenses(Licenses)]),
    ec_talk:say("  Links:~n    ~s", [format_links(Links)]),
    ec_talk:say("  Build tools: ~s", [format_build_tools(BuildTools)]),
    ec_talk:say("Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct", []),
    case ec_talk:ask_default("Proceed?", boolean, "Y") of
        true ->
            upload_package(Auth, PkgName, Version, Meta, FilesAndApp);
        _ ->
            ec_talk:say("Goodbye..."),
            stopped
    end;
publish(_AppDir, _Name, _Version, _Deps, Excluded, _AppDetails) ->
    ?PRV_ERROR({non_hex_deps, Excluded}).

%% Internal functions

validate_app_details(AppDetails) ->
    case proplists:is_defined(contributors, AppDetails) of
        true ->
            {error, {rebar3_hex_pkg, has_contributors}};
        false ->
            ok
    end.

delete(App, Name, Version) ->
    {ok, Auth} = rebar3_hex_config:auth(),
    {application, _, AppDetails} = rebar3_hex_utils:update_app_src(App, Version),
    PkgName = ec_cnv:to_binary(proplists:get_value(pkg_name, AppDetails, Name)),
    case rebar3_hex_http:delete(filename:join([?ENDPOINT, PkgName, "releases", Version]), Auth) of
        ok ->
            rebar_api:info("Successfully deleted package ~s ~s", [PkgName, Version]),
            case ec_talk:ask_default(io_lib:format("Also delete tag v~s?", [Version]), boolean, "N") of
                true ->
                    rebar_utils:sh(io_lib:format("git tag -d v~s", [Version]), []);
                _ ->
                    ok
            end;
        {error, Status} ->
            rebar_api:error("Unable to delete package ~s ~s (~p)", [PkgName, Version, Status])
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
        {error, Status, <<>>} ->
            ?PRV_ERROR({status, Status});
        {error, Status, Error} ->
            ?PRV_ERROR({status, Status, Error})
    end.

errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string(Map) when is_map(Map) ->
    errors_to_string(maps:to_list(Map));
errors_to_string({<<"inserted_at">>, E}) ->
    lists:flatten(io_lib:format("Inserted At: ~s~n", [E]));
errors_to_string({<<"requirements">>,  Rs}) ->
    lists:flatten(["Requirements could not be computed\n",
                  [io_lib:format("~s\n~20.20c\n~s\n",[P,$-, R]) || {P, R} <- maps:to_list(Rs)]]);
errors_to_string({Key, Value}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Value)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Values)]) || Values <- Errors]).

format_deps(Deps) ->
    string:join([binary_to_list(<<N/binary, " ", V/binary>>) || {N, #{<<"requirement">> := V}} <- Deps], "\n    ").

format_maintainers(Maintainers) ->
    string:join(Maintainers, "\n    ").

format_licenses(Licenses) ->
    string:join(Licenses, ", ").

format_links(Links) ->
    string:join([lists:flatten([Name, ": ", Url]) || {Name, Url} <- Links], "\n    ").

format_build_tools(BuildTools) ->
    string:join([io_lib:format("~s", [Tool]) || Tool <- BuildTools], ", ").

update_versions(ConfigDeps, Deps) ->
    [begin
         case lists:keyfind(binary_to_atom(N, utf8), 1, ConfigDeps) of
             {_, V} when is_list(V) ->
                 {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, {<<"requirement">>, list_to_binary(V)}))};
             _ ->
                 %% using version from lock. prepend ~> to make it looser
                 {_, Version} = lists:keyfind(<<"requirement">>, 1, M),
                 {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, {<<"requirement">>, <<"~>", Version/binary>>}))}
         end
     end || {N, M} <- Deps].

-ifdef(TEST).

error_test() ->
    E = #{<<"inserted_at">> => <<"can only modify a release up to one hour after creation">>,
          <<"requirements">> => #{nil => <<"Failed to use \"lager\" (version 3.0.2) because\n  rebar.config requires 3.0.2\n  rebar.config requires ~>3.2.0\n">>}},
    ?assert(is_list(errors_to_string(E))).

-endif.
