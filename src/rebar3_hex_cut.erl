-module(rebar3_hex_cut).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, cut).
-define(DEPS, [{default, lock}]).

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
                                {example, "rebar3 hex cut"},
                                {short_desc, "Increment version number and publish package"},
                                {desc, ""},
                                {opts, [{increment, $i, "increment", {string, undefined},
                                         "Type of semver increment: major, minor or patch"},
                                        rebar3_hex:repo_opt()]}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    case rebar3_hex:task_state(State) of
        {ok, Task} ->
            handle_task(Task);
        {error, Reason} ->
            ?RAISE(Reason)
    end.

%% default path - maybe tag, maybe commit, maybe push commit,  publish, maybe push tag

handle_task(#{args := Args} = Task) ->
    #{repo := Repo, state := State, apps := Apps} = Task,
    Selected = rebar3_hex_io:select_apps(Apps),
    lists:foreach(fun(App) -> cut(State, Repo, App, Args) end, Selected),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error({no_write_key, RepoName}) ->
    io_lib:format("No api key with permissions to write to the repository ~ts was found.", [RepoName]);
format_error({bad_increment, Type}) ->
    io_lib:format("Increment must be major, minor or patch. ~s is not valid.", [Type]);
format_error({invalid_semver, Version}) ->
    Err = "non-semantic version number \"~ts\" found",
    io_lib:format(Err, [Version]);
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Public API
%% ===================================================================

cut(State, Repo, App, #{increment := MaybeType} = Args) ->
    {Version, ResolvedVersion} = version_info(State, App),
    Type = get_increment(MaybeType, ResolvedVersion),
    NewVersion = increment_version(Type, ResolvedVersion),
    AppSrcFile = rebar_app_info:app_file_src(App),

    case Version of
        _Git when Version =:= git orelse Version =:= "git" ->
            create_tag(NewVersion),

            case try_publish(State, Repo, App, Args) of
                {ok, _State} ->
                    case rebar3_hex_io:ask("Push new tag to origin?", boolean, "Y") of
                        true ->
                            maybe_push_tag(Version),
                            {ok, State};
                        false ->
                            {ok, State}
                    end;
                _ ->
                    delete_tag(NewVersion),
                   {ok, State}
            end;
        _ ->
            Spec = rebar3_hex_file:update_app_src(App, NewVersion),
            NewAppSrcFile = io_lib:format("~tp.\n", [Spec]),
            ok = rebar_file_utils:write_file_if_contents_differ(AppSrcFile, NewAppSrcFile),
            ask_commit_and_push(NewVersion),
            case try_publish(State, Repo, rebar_app_info:original_vsn(App, NewVersion), Args) of
                {ok, _} ->
                    maybe_create_and_push_tag(NewVersion),
                    {ok, State};
                {error, publish_failed} ->
                    delete_tag(NewVersion),
                    {ok, State}
            end

    end.

maybe_create_and_push_tag(Version) ->
    PromptStr = io_lib:format("Create new git tag v~s?", [Version]),
    case rebar3_hex_io:ask(PromptStr, boolean, "Y") of
        true ->
            create_tag(Version),
            maybe_push_tag(Version);
        false ->
            ok
    end.

create_tag(Version) ->
    rebar_api:info("Creating new tag v~s...", [Version]),
    rebar_utils:sh(io_lib:format("git tag v~s", [Version]), []).

maybe_push_tag(Version) ->
    case rebar3_hex_io:ask("Push new tag to origin?", boolean, "Y") of
        true ->
            push_tag(Version);
        false ->
            ok
    end.

push_tag(Version) ->
    rebar_api:info("Pushing new tag v~s...", [Version]),
    rebar_utils:sh(io_lib:format("git push origin v~s", [Version]), []).

delete_tag(Version) ->
    rebar_api:info("Deleting new tag v~s...", [Version]),
    rebar_utils:sh(io_lib:format("git tag -d v~s", [Version]), []).

try_publish(State, Repo, App, Args) ->
 try rebar3_hex_publish:publish(State, Repo, App, Args) of
    {ok, _} ->
        {ok, State}
    catch
        error:{error, {rebar3_hex_publish, Err}} ->
        ErrStr  = rebar3_hex_publish:format_error(Err),
        rebar_api:error(ErrStr, []),
        {error, publish_failed}
  end.

increment_version(Type, VersionStr) when is_list(VersionStr) orelse is_binary(VersionStr) ->
    Parsed = parse_version(VersionStr),
    increment_version(Type, Parsed);

increment_version(Type, Version) ->
    rebar3_hex_version:format(rebar3_hex_version:increment(Type, Version)).

get_increment(undefined, Version) ->
    rebar3_hex_io:say("Select semver increment or other (Current ~s):", [Version]),
    rebar3_hex_io:say("1) patch", []),
    rebar3_hex_io:say("2) minor", []),
    rebar3_hex_io:say("3) major", []),
    rebar3_hex_io:say("4) other", []),
    case rebar3_hex_io:ask("[1-4] ", number) of
        4 ->
            rebar3_hex_io:ask("New Version ", string);
        TypeInt ->
            case int_to_increment(TypeInt) of
                error ->
                    rebar_api:error("Invalid number given, try again~n", []),
                    get_increment(undefined, Version);
                Type ->
                    Type
            end
    end;

get_increment(Type, _) -> normalize_increment(Type).

parse_version(Version) ->
    case rebar3_hex_version:parse(Version) of
        {ok, Parsed} ->
            Parsed;
        _Err ->
         ?RAISE({invalid_semver, Version})
    end.

int_to_increment(1) -> patch;
int_to_increment(2) -> minor;
int_to_increment(3) -> major;
int_to_increment(_) -> error.

version_info(State, App) ->
    Version = rebar_app_info:original_vsn(App),
    Resolved = rebar3_hex_app:vcs_vsn(State, App),
    {Version, Resolved}.

normalize_increment("patch") -> patch;
normalize_increment("minor") -> minor;
normalize_increment("major") -> major;
normalize_increment(_) -> error.

ask_commit_and_push(NewVersion) ->
    case rebar3_hex_io:ask(io_lib:format("Create 'v~s' commit?", [NewVersion]), boolean, "Y") of
        true ->
            rebar_utils:sh(io_lib:format("git commit -a -m 'v~s'", [NewVersion]), []),
            case rebar3_hex_io:ask("Push master to origin master?", boolean, "N") of
                true ->
                    rebar_utils:sh("git push origin master:master", []);
                false ->
                    ok
            end;
        false ->
            ok
    end.
