%% @doc The publish provider is responsible for creating a tarball of
%% an application and uploading to the repository.
%% @end
-module(rebar3_hex_publish).

-export([ init/1
        , do/1
        , format_error/1
        ]).

-export([
         publish/4,
         publish_package/4
        , publish_docs/4
        ]).

-include("rebar3_hex.hrl").

-define(PROVIDER, publish).
-define(DEPS, [{default, lock}]).

-define(DEFAULT_FILES, ["src", "c_src", "include", "rebar.config.script"
                       ,"priv", "rebar.config", "rebar.lock"
                       ,"CHANGELOG*", "changelog*"
                       ,"README*", "readme*"
                       ,"LICENSE*", "license*"
                       ,"NOTICE"]).

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
                                 {desc, support()},
                                 {opts, [rebar3_hex:repo_opt(),
                                         {yes, $y, "yes", {boolean, false}, help(yes)},
                                         {app, $a, "app", {string, undefined}, help(app)},
                                         {replace, undefined, "replace", {boolean, false}, help(replace)},
                                         {revert, undefined, "revert", string, help(revert)}]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, term()}.
do(State) ->
    case rebar3_hex:task_state(State) of
        {ok, Task} ->
            handle_task(Task);
        {error, Reason} ->
            ?RAISE(Reason)
    end.

-spec format_error(any()) -> iolist().
format_error(ErrList) when is_list(ErrList) ->
  F = fun(Err, Acc) ->
          ErrStr = format_error(Err),
          Acc ++ "     " ++ ErrStr ++ "\n"
      end,
  More = "\n     Please see https://hex.pm/docs/rebar3_publish for more info.\n",
  lists:foldl(F, "Validator Errors:\n", ErrList) ++ More;
format_error({validation_errors, Errs}) ->
    lists:map(fun(E) -> format_error(E) end, Errs);
format_error({has_contributors, AppName}) ->
    Err = "~ts.app.src : deprecated field contributors found",
    io_lib:format(Err, [AppName]);
format_error({has_maintainers, AppName}) ->
    Err = "~ts.app.src : deprecated field maintainers found",
    io_lib:format(Err, [AppName]);
format_error({no_description, AppName}) ->
    Err = "~ts.app.src : missing or empty description property",
    io_lib:format(Err, [AppName]);
format_error({no_license, AppName}) ->
    Err = "~ts.app.src : missing or empty licenses property",
    io_lib:format(Err, [AppName]);
format_error({invalid_semver, {AppName, Version}}) ->
    Err = "~ts.app.src : non-semantic version number \"~ts\" found",
    io_lib:format(Err, [AppName, Version]);
format_error({has_unstable_deps, Deps}) ->
    MainMsg = "The following pre-release dependencies were found : ",
    DepList = [io_lib:format("~s - ~s ", [Pkg, Ver]) || {Pkg, Ver} <- Deps],
    Msg = [
        "In the future packages with pre-release dependencies will be considered unstable ",
        "and will be prevented from being published. ",
        "We recommend you upgrade your these dependencies as soon as possible"
    ],
    io_lib:format("~s~n~n~s~n~n~s~n", [MainMsg, DepList, Msg]);

format_error({app_not_found, AppName}) ->
     io_lib:format("App ~s specified with --app switch not found in project", [AppName]);
format_error(bad_command) ->
        "bad command";
format_error({app_switch_required, Msg}) ->
    Msg;
format_error({required, repo}) ->
    "publish requires a repo name argument to identify the repo to publish to";
format_error({not_valid_repo, RepoName}) ->
    io_lib:format("No configuration for repository ~ts found.", [RepoName]);
format_error(no_write_key) ->
    "No write key found for user. Be sure to authenticate first with:"
    ++ " rebar3 hex user auth";
format_error({publish, {error, {tarball, _} = Err}}) ->
    hex_tarball:format_error(Err);
format_error({publish, {error, #{<<"errors">> := Errors, <<"message">> := Message}}}) ->
    ErrorString = errors_to_string(Errors),
    io_lib:format("Failed to publish package: ~ts~n\t~ts", [Message, ErrorString]);
format_error({publish, {error, #{<<"message">> := Message}}}) ->
    io_lib:format("Failed to publish package: ~ts", [Message]);
format_error({non_hex_deps, Excluded}) ->
    Err = "Can not publish package because the following deps are not available"
         ++ " in hex: ~s",
    io_lib:format(Err, [string:join(Excluded, ", ")]);
format_error(undefined_server_error) ->
    "Unknown server error";
format_error({status, Status}) ->
    rebar3_hex_client:pretty_print_status(Status);
format_error({status, Status, undefined_server_error}) ->
    "Unknown server error: " ++ rebar3_hex_client:pretty_print_status(Status);
format_error({status, Status, Error}) ->
  Message = maps:get(<<"message">>, Error, ""),
  Errors = maps:get(<<"errors">>, Error, ""),
  ErrorString = errors_to_string(Errors),
  Data =  [rebar3_hex_client:pretty_print_status(Status), Message, ErrorString],
  io_lib:format("Status Code: ~s~nHex Error: ~s~n\t~s", Data);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Private
%% ===================================================================


%% ===================================================================
%% Publish package only operations
%% ===================================================================
handle_task(#{args := #{task := package, app := undefined}, multi_app := true}) ->
    ?RAISE({app_switch_required, "--app required when publishing with the package argument in a umbrella"});

handle_task(#{args := #{task := package}, apps := [App]} = Task) ->
    maybe_warn_about_single_app_args(Task),
    #{args := Args, repo := Repo, state := State} = Task,
    publish_package(State, Repo, App, Args);

handle_task(#{args := #{task := package, app := AppName}, apps := Apps} = Task) ->
    #{args := Args, repo := Repo, state := State} = Task,
    case rebar3_hex_app:find(Apps, AppName) of
        {error, app_not_found} ->
            ?RAISE({app_not_found, AppName});
        {ok, App} ->
            publish_package(State, Repo, App, Args)
    end,
    {ok, State};

%% ===================================================================
%% Publish docs only operations
%% ===================================================================
%% TODO: Move doc publish code into this module
handle_task(#{args := #{task := docs, app := undefined}, multi_app := true}) ->
    ?RAISE({app_switch_required, "--app required when running publish docs only in a umbrella"});

handle_task(#{args := #{task := docs}, apps := [App]} = Task) ->
    #{args := Args, repo := Repo, state := State} = Task,
    publish_docs(State, Repo, App, Args),
    {ok, State};

handle_task(#{args := #{task := docs, app := AppName}, apps := Apps} = Task) ->
    case rebar3_hex_app:find(Apps, AppName) of
        {error, app_not_found} ->
            ?RAISE({app_not_found, AppName});
        {ok, App} ->
            #{args := Args, repo := Repo, state := State} = Task,
            publish_docs(App, State, Repo, Args),
            {ok, State}
    end;

%% ===================================================================
%% Revert operations
%% ===================================================================
%% TODO: Move revert code into this module
handle_task(#{args := #{revert := undefined}}) ->
    {error, "--revert requires an app version"};

handle_task(#{args := #{revert := _, app := undefined}, multi_app := true}) ->
    {error, "--app required when reverting in a umbrella with multiple apps"};

handle_task(#{args := #{revert := Vsn}, apps := [App]} = Task) ->
    #{repo := Repo, state := State} = Task,
    AppName = rebar_app_info:name(App),
    revert(State, Repo, AppName, Vsn);

handle_task(#{args := #{revert := Vsn, app := AppName}, apps := Apps} = Task) ->
    #{repo := Repo, state := State} = Task,
    case rebar3_hex_app:find(Apps, AppName) of
        {error, app_not_found} ->
            ?RAISE({app_not_found, AppName});
        {ok, _App} ->
            revert(State, Repo, AppName, Vsn)
    end;

%% ===================================================================
%% Publish package and docs (the default path)
%% ===================================================================
handle_task(#{args := #{app := undefined}, repo := Repo, state := State, apps := Apps} = Task) ->
    #{args := Args} = Task,
    maybe_warn_about_single_app_args(Task),
    Selected = rebar3_hex_io:select_apps(Apps),
    lists:foreach(fun(App) -> publish(State, Repo, App, Args) end, Selected),
    {ok, State};

handle_task(#{args := #{app := AppName},  apps := Apps, multi_app := true} = Task) ->
    case rebar3_hex_app:find(Apps, AppName) of
        {error, app_not_found} ->
            ?RAISE({app_not_found, AppName});
        {ok, App} ->
            #{args := Args, repo := Repo, state := State} = Task,
            publish(State, Repo, App, Args)
    end.

maybe_warn_about_single_app_args(#{args := #{app := AppName}, apps := [_]}) when AppName =/= undefined ->
    rebar_api:error("--app switch has no effect in single app projects", []);
maybe_warn_about_single_app_args(_) -> ok.

publish(State, Repo, App, Args) ->
    case publish_package(State, Repo, App, Args) of 
        abort -> 
            {ok, State};
        _ -> 
            publish_docs(State, Repo, App, Args)
    end.

publish_docs(State, Repo, App, _Args) ->
    rebar3_hex_docs:publish(App, State, Repo).

publish_package(State, Repo, App, Args) ->
    assert_valid_app(State, App),
    Package = build_package(State, Repo, App),
    print_package_info(Package),
    maybe_say_coc(Repo),
    MaybeCheckoutWarnings = maybe_checkout_warnings(App),
    case maybe_prompt(Args, "Proceed" ++ MaybeCheckoutWarnings ++ "?") of
        proceed ->
            HexOpts = hex_opts(Args),
            assert_has_write_key(Repo),
            {ok, HexConfig} = rebar3_hex_config:hex_config_write(Repo),
            rebar_api:info("package argument given, will not publish docs", []),
            Tarball = create_tarball(Package),
            case rebar3_hex_client:publish(HexConfig, Tarball, HexOpts) of
                {ok, _Res} ->
                    #{name := Name, version := Version} = Package,
                        rebar_api:info("Published ~s ~s", [Name, Version]),
                        {ok, State};
                Error ->
                    ?RAISE({publish, Error})
            end;
        abort ->
            rebar3_hex_io:say("Goodbye..."),
            abort
    end.

revert(State, Repo, AppName, Vsn) -> 
    case rebar3_hex_revert:revert(binarify(AppName), binarify(Vsn), Repo, State) of
        ok ->
            {ok, State};
        Err ->
            ?RAISE(Err)
    end.

build_package(State, #{name := RepoName} = _Repo, App) ->
    Name = rebar_app_info:name(App),

    Version = rebar3_hex_app:vcs_vsn(State, App),

    %% Note we should not implicitly do this IMO
    {application, _, AppDetails} = rebar3_hex_file:update_app_src(App, Version),

    Deps = rebar_state:get(State, {locks, default}, []),
    TopLevel = gather_deps(Deps),
    AppDir = rebar_app_info:dir(App),
    Config = rebar_config:consult(AppDir),
    ConfigDeps = proplists:get_value(deps, Config, []),
    Deps1 = update_versions(ConfigDeps, TopLevel),
    Description = proplists:get_value(description, AppDetails, ""),
    PackageFiles = include_files(Name, AppDir, AppDetails),
    Licenses = proplists:get_value(licenses, AppDetails, []),
    Links = proplists:get_value(links, AppDetails, []),
    BuildTools = proplists:get_value(build_tools, AppDetails, [<<"rebar3">>]),

    %% We check the app file for the 'pkg' key which allows us to select
    %% a package name other then the app name, if it is not set we default
    %% back to the app name.
    PkgName = rebar_utils:to_binary(proplists:get_value(pkg_name, AppDetails, Name)),

    Optional = [{<<"app">>, Name},
                {<<"parameters">>, []},
                {<<"description">>, rebar_utils:to_binary(Description)},
                {<<"files">>, [binarify(File) || {File, _} <- PackageFiles]},
                {<<"licenses">>, binarify(Licenses)},
                {<<"links">>, to_map(binarify(Links))},
                {<<"build_tools">>, binarify(BuildTools)}],
    OptionalFiltered = [{Key, Value} || {Key, Value} <- Optional, Value =/= []],
    Metadata = maps:from_list([{<<"name">>, PkgName}, {<<"version">>, binarify(Version)},
                               {<<"requirements">>, maps:from_list(Deps1)} | OptionalFiltered]),
    #{name => PkgName,
      repo_name => RepoName,
      deps => Deps1,
      version => Version,
      metadata => Metadata,
      files => PackageFiles}.

print_package_info(Package) ->
    #{metadata := Meta, files := Files, deps := Deps, name := Name, repo_name := RepoName, version := Version} = Package,
    rebar3_hex_io:say("Publishing ~ts ~ts to ~ts", [Name, Version, RepoName]),
    rebar3_hex_io:say("  Description: ~ts", [rebar_utils:to_list(maps:get(<<"description">>, Meta))]),
    rebar3_hex_io:say("  Dependencies:~n    ~ts", [format_deps(Deps)]),
    rebar3_hex_io:say("  Included files:~n    ~ts", [string:join([F || {F, _} <- Files], "\n    ")]),
    rebar3_hex_io:say("  Licenses: ~ts", [format_licenses(maps:get(<<"licenses">>, Meta))]),
    rebar3_hex_io:say("  Links:~n    ~ts", [format_links(maps:get(<<"links">>, Meta))]),
    rebar3_hex_io:say("  Build tools: ~ts", [format_build_tools(maps:get(<<"build_tools">>, Meta))]).

maybe_checkout_warnings(App) ->
    case has_checkouts_for(rebar_app_info:dir(App)) of
        {false, _} -> "";
        {true, _} -> " (with warnings)"
    end.

maybe_prompt(#{yes := true}, _Message) ->
    proceed;

maybe_prompt(_Args, Message) ->
    case rebar3_hex_io:ask(Message, boolean, "Y") of
        true ->
            proceed;
        _ ->
            abort
    end.

create_tarball(#{metadata := Meta, files := Files}) ->
    case hex_tarball:create(Meta, Files) of
        {ok, #{tarball := Tarball, inner_checksum := _Checksum}} ->
            Tarball;
        Error ->
            ?RAISE(Error)
    end.

assert_valid_app(State, App) ->
    Name = rebar_app_info:name(App),
    Version = rebar_app_info:original_vsn(App),
    ResolvedVersion = rebar_utils:vcs_vsn(App, Version, State),
    {application, _, AppDetails} = rebar3_hex_file:update_app_src(App, ResolvedVersion),
    Deps = rebar_state:get(State, {locks, default}, []),
    AppData = #{name => Name, version => ResolvedVersion, details => AppDetails, deps => Deps},
    case rebar3_hex_app:validate(AppData) of
        ok ->
            {ok, State};
       {error, #{warnings := Warnings, errors := Errors}} -> 
            lists:foreach(fun(W) -> rebar_log:log(warn, format_error(W), []) end, Warnings),
            case Errors of 
                [] -> 
                    {ok, State};
                Errs -> 
                    ?RAISE({validation_errors, Errs})
            end
    end.

hex_opts(Opts) ->
    lists:filter(fun({K, _}) -> is_hex_opt(K) end, maps:to_list(Opts)).

is_hex_opt(replace) -> true;
is_hex_opt(_) -> false.

gather_deps(Deps) ->
    case rebar3_hex_app:get_deps(Deps) of
        {ok, Top} ->
            Top;
        {error, Reason} ->
             ?RAISE(Reason)
    end.

%% Internal functions

%% if publishing to the public repo or to a private organization link to the code of conduct
maybe_say_coc(#{parent := <<"hexpm">>}) ->
    rebar3_hex_io:say("Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct", []);
maybe_say_coc(#{name := <<"hexpm">>}) ->
    rebar3_hex_io:say("Be aware, you are publishing to the public Hexpm repository.", []),
    rebar3_hex_io:say("Before publishing, please read Hex CoC: https://hex.pm/policies/codeofconduct", []);
maybe_say_coc(_) ->
    ok.

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

assert_has_write_key(Repo) ->
    MaybeApiKey = maps:get(api_key, Repo, undefined),
    case maps:get(write_key, Repo, MaybeApiKey) of
        undefined ->
            ?RAISE(no_write_key);
        _ ->
            ok
    end.

%% allows us to support lists of tuples or maps for metadata the user writes in .app.src
to_map(Map) when is_map(Map) ->
    Map;
to_map(List) when is_list(List) ->
    maps:from_list(List).

include_files(Name, AppDir, AppDetails) ->
    _ = maybe_print_checkouts_warnings(AppDir),

    AppSrc = {application, to_atom(Name), AppDetails},
    FilePaths = proplists:get_value(files, AppDetails, ?DEFAULT_FILES),
    IncludeFilePaths = proplists:get_value(include_files, AppDetails, []),
    ExcludeFilePaths = proplists:get_value(exclude_files, AppDetails, []),
    ExcludeRes = proplists:get_value(exclude_regexps, AppDetails, []),

    AllFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(FilePaths, AppDir)),
    IncludeFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(IncludeFilePaths, AppDir)),
    ExcludeFiles = lists:ukeysort(2, rebar3_hex_file:expand_paths(ExcludeFilePaths, AppDir)),

    %% We filter first and then include, that way glob excludes can be
    %% overwritten be explict includes
    FilterExcluded = lists:filter(fun ({_, Path}) ->
                                      not exclude_file(Path, ExcludeFiles, ExcludeRes)
                                  end, AllFiles),
    WithIncludes = lists:ukeymerge(2, FilterExcluded, IncludeFiles),

    AppFileSrc = filename:join("src", rebar_utils:to_list(Name)++".app.src"),
    AppSrcBinary = rebar_utils:to_binary(lists:flatten(io_lib:format("~tp.\n", [AppSrc]))),
    lists:keystore(AppFileSrc, 1, WithIncludes, {AppFileSrc, AppSrcBinary}).

maybe_print_checkouts_warnings(AppDir) ->
    {HasCheckouts, Checkouts} = has_checkouts_for(AppDir),
    HasCheckouts andalso
        rebar_log:log(warn, "~p directory found; this might interfere with publishing", [Checkouts]).

has_checkouts_for(AppDir) ->
    Checkouts = filename:join(AppDir, "_checkouts"),
    {filelib:is_dir(Checkouts), Checkouts}.

format_deps(Deps) ->
    Res = [rebar_utils:to_list(<<N/binary, " ", V/binary>>) || {N, #{<<"requirement">> := V}} <- Deps],
    string:join(Res, "\n    ").

format_licenses(Licenses) ->
    string:join([rebar_utils:to_list(L) || L <- Licenses], ", ").

format_links(Links) ->
    Links1 = maps:to_list(Links),
    LinksList = [lists:flatten([rebar_utils:to_list(Name), ": ", rebar_utils:to_list(Url)]) || {Name, Url} <- Links1],
    string:join(LinksList, "\n    ").

format_build_tools(BuildTools) ->
    string:join([io_lib:format("~s", [Tool]) || Tool <- BuildTools], ", ").

update_versions(ConfigDeps, Deps) ->
    [begin
         case lists:keyfind(binary_to_atom(N, utf8), 1, ConfigDeps) of
             {_, V} when is_binary(V) ->
                 Req =  {<<"requirement">>, V},
                 {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, Req))};
             {_, V} when is_list(V) ->
                 Req = {<<"requirement">>, rebar_utils:to_binary(V)},
                 {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, Req))};
             _ ->
                 %% using version from lock. prepend ~> to make it looser
                 {_, Version} = lists:keyfind(<<"requirement">>, 1, M),
                 Req = {<<"requirement">>, <<"~>", Version/binary>>},
                 {N, maps:from_list(lists:keyreplace(<<"requirement">>, 1, M, Req))}
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


binarify(Term) when is_boolean(Term) ->
    Term;
binarify(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
binarify([]) ->
    [];
binarify(Map) when is_map(Map) ->
    maps:from_list(binarify(maps:to_list(Map)));
binarify(Term) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true ->
            rebar_utils:to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.

-spec to_atom(atom() | string() | binary() | integer() | float()) ->
                     atom().
to_atom(X) when erlang:is_atom(X) ->
    X;
to_atom(X) when erlang:is_list(X) ->
    list_to_existing_atom(X);
to_atom(X) ->
    to_atom(rebar_utils:to_list(X)).


help(app) ->
    "Specifies the app to use with the publish command, currently only utilized for publish and revert operations"
    "Note that the app switch and value only have to be provided if you are publishing within an umbrella.";
help(revert) ->
    "Revert given version, if the last version is reverted the package is removed";
help(replace) ->
    "Allows overwriting an existing package version if it exists. Private "
    "packages can always be overwritten, publicpackages can only be "
    "overwritten within one hour after they were initially published.";
help(yes) ->
    "Publishes the package without any confirmation prompts".

support() ->
    "Publishes a new version of a package with options to revert and replace existing packages~n~n"
    "Supported commmand combinations:~n~n"
    "  rebar3 hex publish~n~n"
    "  rebar3 hex publish package~n~n"
    "  rebar3 hex publish --yes~n~n"
    "  rebar3 hex publish package~n~n"
    "  rebar3 hex publish docs~n~n"
    "  rebar3 hex publish --repo <repo>~n~n"
    "  rebar3 hex publish --repo <repo> --yes~n~n"
    "  rebar3 hex publish --revert <version>~n~n"
    "  rebar3 hex publish --revert <version> --yes~n~n"
    "  rebar3 hex publish --revert <version> --app <app>~n~n"
    "  rebar3 hex publish --revert <version> --app <app> --yes~n~n"
    "  rebar3 hex publish --replace~n~n"
    "  rebar3 hex publish --replace --yes~n~n"
    "Argument descriptions:~n~n"
    "  <repo>    - a valid repository, only required when multiple repositories are configured~n~n"
    "  <version> - a valid version string, currently only utilized with --revert switch~n~n".
