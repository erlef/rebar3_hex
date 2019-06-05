%% @doc The publish provider is responsible for creating a tarball of
%% an application and uploading to the repository.
%% @end
-module(rebar3_hex_publish).

-export([init/1,
         do/1,
         format_error/1]).

-export([publish/3
        ,publish/8,
        validate_app_details/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, publish).
-define(DEPS, [{default, lock}]).

-define(VALIDATIONS, [  has_semver
                      , has_contributors
                      , has_maintainers
                      , has_description
                      , has_licenses ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
                                 {example, "rebar3 hex publish"},
                                 {short_desc, "Publish a new version of your package and update the package"},
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
    case maps:get(write_key, Repo, undefined) of
        undefined ->
            ?PRV_ERROR(no_write_key);
        _ ->
            Apps = rebar3_hex_utils:select_apps(rebar_state:project_apps(State)),
            lists:foldl(fun(App, {ok, StateAcc}) ->
                                publish(App, Repo, StateAcc)
                        end, {ok, State}, Apps)
    end.

-spec format_error(any()) -> iolist().
format_error(ErrList) when is_list(ErrList) ->
  F = fun(Err, Acc) ->
          ErrStr = format_error(Err),
          Acc ++ "     " ++ ErrStr ++ "\n"
      end,
  More = "\n     Please see https://hex.pm/docs/rebar3_publish for more info.\n",
  lists:foldl(F, "Validator Errors:\n", ErrList) ++ More;
format_error({required, repo}) ->
    "publish requires a repo name argument to identify the repo to publish to";
format_error({not_valid_repo, RepoName}) ->
    io_lib:format("No configuration for repository ~ts found.", [RepoName]);
format_error({invalid_semver, AppName, Version}) ->
    Err = "~ts.app.src : non-semantic version number \"~ts\" found",
    io_lib:format(Err, [AppName, Version]);
format_error({no_description, AppName}) ->
    Err = "~ts.app.src : missing or empty description property",
    io_lib:format(Err, [AppName]);
format_error({no_license, AppName}) ->
    Err = "~ts.app.src : missing or empty licenses property",
    io_lib:format(Err, [AppName]);
format_error({has_maintainers, AppName}) ->
    Err = "~ts.app.src : deprecated field maintainers found",
    io_lib:format(Err, [AppName]);
format_error({has_contributors, AppName}) ->
    Err = "~ts.app.src : deprecated field contributors found",
    io_lib:format(Err, [AppName]);
format_error(no_write_key) ->
    "No write key found for user. Be sure to authenticate first with:"
    ++ " rebar3 hex user auth";
format_error({validation_errors, Errors, Message}) ->
    ErrorString = errors_to_string(Errors),
    io_lib:format("Failed to publish package: ~ts~n\t~ts", [Message, ErrorString]);
format_error({publish_failed, Message}) ->
    io_lib:format("Failed to publish package: ~ts", [Message]);
format_error({non_hex_deps, Excluded}) ->
    Err = "Can not publish package because the following deps are not available"
         ++ " in hex: ~s",
    io_lib:format(Err, [string:join(Excluded, ", ")]);
format_error(undefined_server_error) ->
    "Unknown server error";
format_error({status, Status}) ->
    rebar3_hex_utils:pretty_print_status(Status);
format_error({status, Status, undefined_server_error}) ->
    "Unknown server error: " ++ rebar3_hex_utils:pretty_print_status(Status);
format_error({status, Status, Error}) ->
  Message = maps:get(<<"message">>, Error, ""),
  Errors = maps:get(<<"errors">>, Error, ""),
  ErrorString = errors_to_string(Errors),

  io_lib:format("Status Code: ~s~nHex Error: ~s~n\t~s", [rebar3_hex_utils:pretty_print_status(Status),
                                                         Message, ErrorString]).

%% ===================================================================
%% Public API
%% ===================================================================

publish(App, HexConfig, State) ->
    AppDir = rebar_app_info:dir(App),
    Name = rebar_app_info:name(App),

    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(Version,
                                          rebar_app_info:dir(App),
                                          rebar_state:resources(State)),
    {application, _, AppDetails} = rebar3_hex_utils:update_app_src(App, ResolvedVersion),


    Deps = rebar_state:get(State, {locks, default}, []),
    TopLevel = [{N, [{<<"app">>, A},
                     {<<"optional">>, false},
                     {<<"requirement">>, V}]} || {A,{pkg,N,V,_},0} <- Deps],
    Excluded = [binary_to_list(N) || {N,{T,_,_},0} <- Deps, T =/= pkg],

    case is_valid_app({App, Name, Version, AppDetails}) of
        ok ->
            publish(AppDir, Name, ResolvedVersion, TopLevel,
                    Excluded, AppDetails, HexConfig, State);
        {error, Errors} ->
            ?PRV_ERROR(Errors)
    end.

publish(AppDir, Name, Version, Deps, [], AppDetails, HexConfig, State) ->
    Config = rebar_config:consult(AppDir),
    ConfigDeps = proplists:get_value(deps, Config, []),
    Deps1 = update_versions(ConfigDeps, Deps),

    Description = proplists:get_value(description, AppDetails, ""),

    PackageFiles = include_files(Name, AppDir, AppDetails),

    Licenses = proplists:get_value(licenses, AppDetails, []),
    Links = proplists:get_value(links, AppDetails, []),
    BuildTools = proplists:get_value(build_tools, AppDetails, [<<"rebar3">>]),

    %% We check the app file for the 'pkg' key wich allows us to select
    %% a package name other then the app name, if it is not set we default
    %% back to the app name.
    PkgName = ec_cnv:to_binary(proplists:get_value(pkg_name, AppDetails, Name)),

    Optional = [{<<"app">>, Name},
                {<<"parameters">>, []},
                {<<"description">>, unicode:characters_to_binary(Description)},
                {<<"files">>, [rebar3_hex_utils:binarify(File) || {File, _} <- PackageFiles]},
                {<<"licenses">>, rebar3_hex_utils:binarify(Licenses)},
                {<<"links">>, to_map(rebar3_hex_utils:binarify(Links))},
                {<<"build_tools">>, rebar3_hex_utils:binarify(BuildTools)}],
    OptionalFiltered = [{Key, Value} || {Key, Value} <- Optional, Value =/= []],
    Metadata = maps:from_list([{<<"name">>, PkgName}, {<<"version">>, rebar3_hex_utils:binarify(Version)},
                               {<<"requirements">>, maps:from_list(Deps1)} | OptionalFiltered]),

    ec_talk:say("Publishing ~ts ~ts to ~ts", [PkgName, Version, maps:get(name, HexConfig)]),
    ec_talk:say("  Description: ~ts", [Description]),
    ec_talk:say("  Dependencies:~n    ~ts", [format_deps(Deps1)]),
    ec_talk:say("  Included files:~n    ~ts", [string:join([F || {F, _} <- PackageFiles], "\n    ")]),
    ec_talk:say("  Licenses: ~ts", [format_licenses(Licenses)]),
    ec_talk:say("  Links:~n    ~ts", [format_links(Links)]),
    ec_talk:say("  Build tools: ~ts", [format_build_tools(BuildTools)]),
    maybe_say_coc(HexConfig),
    case ec_talk:ask_default("Proceed?", boolean, "Y") of
        true ->
            Username = maps:get(username, HexConfig),
            WriteKey = maps:get(write_key, HexConfig),
            HexConfig1 = HexConfig#{api_key => rebar3_hex_user:decrypt_write_key(Username, WriteKey)},
            case create_and_publish(Metadata, PackageFiles, HexConfig1) of
                ok ->
                    rebar_api:info("Published ~s ~s", [Name, Version]),
                    {ok, State};
                Error={error, _} ->
                    Error
            end;
        _ ->
            ec_talk:say("Goodbye..."),
            {ok, State}
    end;
publish(_AppDir, _Name, _Version, _Deps, Excluded, _AppDetails, _, _) ->
    ?PRV_ERROR({non_hex_deps, Excluded}).

%% Internal functions

%% if publishing to the public repo or to a private organization link to the code of conduct
maybe_say_coc(#{parent := <<"hexpm">>}) ->
    ec_talk:say("Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct", []);
maybe_say_coc(#{name := <<"hexpm">>}) ->
    ec_talk:say("Be aware, you are publishing to the public Hexpm repository.", []),
    ec_talk:say("Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct", []);
maybe_say_coc(_) ->
    ok.

create_and_publish(Metadata, PackageFiles, HexConfig) ->
    {ok, {Tarball, _Checksum}} = hex_tarball:create(Metadata, PackageFiles),
    case hex_api_release:publish(HexConfig, Tarball) of
        {ok, {400, _Headers, #{<<"message">> := Message}}} ->
            ?PRV_ERROR({publish_failed, Message});
        {ok, {401, _Headers, #{<<"message">> := Message}}} ->
            ?PRV_ERROR({publish_failed, Message});
        {ok, {422, _Headers, #{<<"errors">> := Errors,
                               <<"message">> := Message}}} ->
            ?PRV_ERROR({validation_errors, Errors, Message});
        {ok, {201, _Headers, _Body}} ->
            ok;
        {ok, {200, _Headers, _Body}} ->
            ok;
        {error, Reason} ->
            ?PRV_ERROR({error, Reason})
    end.


known_exclude_file(Path, ExcludeRe) ->
    KnownExcludes = [
                     "~$",        %% emacs temp files
                     "\\.o$",     %% c object files
                     "\\.so$",    %% compiled nif libraries
                     "\\.swp$"    %% vim swap files
                    ],
    lists:foldl(fun(_, true) -> true;
                   (RE, false) ->
                        re:run(Path, RE) =/= nomatch
                end, false, KnownExcludes ++ ExcludeRe).

exclude_file(Path, ExcludeFiles, ExcludeRe) ->
    lists:keymember(Path, 2, ExcludeFiles) orelse
        known_exclude_file(Path, ExcludeRe).

%% allows us to support lists of tuples or maps for metadata the user writes in .app.src
to_map(Map) when is_map(Map) ->
    Map;
to_map(List) when is_list(List) ->
    maps:from_list(List).

include_files(Name, AppDir, AppDetails) ->
    AppSrc = {application, ec_cnv:to_atom(Name), AppDetails},
    FilePaths = proplists:get_value(files, AppDetails, ?DEFAULT_FILES),
    IncludeFilePaths = proplists:get_value(include_files, AppDetails, []),
    ExcludeFilePaths = proplists:get_value(exclude_files, AppDetails, []),
    ExcludeRes = proplists:get_value(exclude_regexps, AppDetails, []),

    AllFiles = lists:ukeysort(2, rebar3_hex_utils:expand_paths(FilePaths, AppDir)),
    IncludeFiles = lists:ukeysort(2, rebar3_hex_utils:expand_paths(IncludeFilePaths, AppDir)),
    ExcludeFiles = lists:ukeysort(2, rebar3_hex_utils:expand_paths(ExcludeFilePaths, AppDir)),

    %% We filter first and then include, that way glob excludes can be
    %% overwritten be explict includes
    FilterExcluded = lists:filter(fun ({_, Path}) ->
                                      not exclude_file(Path, ExcludeFiles, ExcludeRes)
                                  end, AllFiles),
    WithIncludes = lists:ukeymerge(2, FilterExcluded, IncludeFiles),

    AppFileSrc = filename:join("src", ec_cnv:to_list(Name)++".app.src"),
    AppSrcBinary = ec_cnv:to_binary(lists:flatten(io_lib:format("~tp.\n", [AppSrc]))),
    lists:keyreplace(AppFileSrc, 1, WithIncludes, {AppFileSrc, AppSrcBinary}).


is_valid_app({_App, _Name, _Version, _AppDetails} = A) ->
    F = fun(K, Acc) ->
            case validate_app(K, A) of
                ok ->
                    Acc;
                {error, Error} ->
                    Acc ++ [Error]
            end
        end,
    case lists:foldl(F, [], ?VALIDATIONS) of
        [] ->
            ok;
        Errors ->
            {error, Errors}
    end.

validate_app(has_semver, {_, Name, Ver, _}) ->
    case ec_semver_parser:parse(Ver) of
        {fail, _} ->
            {error, {invalid_semver, Name, Ver}};
        _ ->
         ok
    end;
validate_app(has_contributors, {_, Name, _, AppDetails}) ->
    case proplists:is_defined(contributors, AppDetails) of
        true ->
            {error, {has_contributors, Name}};
        false ->
            ok
    end;
validate_app(has_maintainers, {_, Name, _, AppDetails}) ->
    case proplists:is_defined(maintainers, AppDetails) of
        true ->
            {error, {has_maintainers, Name}};
        false ->
            ok
    end;
validate_app(has_description, {_, Name, _, AppDetails}) ->
    case is_empty_prop(description, AppDetails) of
        true ->
            {error, {no_description, Name}};
        false ->
            ok
    end;
validate_app(has_licenses, {_, Name, _, AppDetails}) ->
    case is_empty_prop(licenses, AppDetails)  of
        true ->
          {error, {no_license, Name}};
        _ ->
          ok
    end.

is_empty_prop(K, PropList) ->
    Prop = proplists:get_value(K, PropList),
    case Prop of
        Empty when Empty =:= [] orelse Empty =:= undefined ->
          true;
        _ ->
          false
    end.

%% TODO: Modify hex cut so we can deprecate this?
validate_app_details(AppDetails) ->
    case proplists:is_defined(contributors, AppDetails) of
        true ->
            {error, {rebar3_hex_publish, has_contributors}};
        false ->
            ok
    end.

format_deps(Deps) ->
    string:join([binary_to_list(<<N/binary, " ", V/binary>>) || {N, #{<<"requirement">> := V}} <- Deps], "\n    ").

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
