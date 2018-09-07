-module(rebar3_hex_utils).

-export([pretty_print_status/1,
         repo_opt/0,
         repo/1,
         format_error/1,
         update_app_src/2,
         select_apps/1,
         binarify/1,
         expand_paths/2]).

-include("rebar3_hex.hrl").

pretty_print_status(401) -> "Authentication failed (401)";
pretty_print_status(403) -> "Forbidden (403)";
pretty_print_status(404) -> "Entity not found (404)";
pretty_print_status(422) -> "Validation failed (422)";
pretty_print_status(Code) -> io_lib:format("HTTP status code: ~p", [Code]).

repo_opt() ->
    {repo, $r, "repo", string, "Repository to use for this command."}.

repo(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource:find_resource_state(pkg, Resources),
    case proplists:get_value(repo, Args, undefined) of
        undefined ->
            [Repo | _] = Repos,
            Repo;
        RepoName ->
            case rebar_hex_repos:get_repo_config(rebar_utils:to_binary(RepoName), Repos) of
                {ok, Repo} ->
                    Repo;
                _ ->
                    throw(?PRV_ERROR({not_valid_repo, RepoName}))
            end
    end.

format_error({not_valid_repo, RepoName}) ->
    io_lib:format("No configuration for repository ~ts found.", [RepoName]).

update_app_src(App, Version) ->
    AppSrcFile = rebar_app_info:app_file_src(App),
    AppSrc = rebar_file_utils:try_consult(AppSrcFile),
    [{application, Name, Details}] = AppSrc,

    NewDetails = lists:keyreplace(vsn, 1, Details, {vsn, ec_cnv:to_list(Version)}),

    {application, Name, NewDetails}.

select_apps([App]) ->
    [App];
select_apps(Apps) ->
    ec_talk:say("Select application(s):", []),
    lists:foldl(fun(App, Idx) ->
                        ec_talk:say("~p) ~s", [Idx, rebar_app_info:name(App)]),
                        Idx+1
                end, 1, Apps),
    ec_talk:say("------------", []),
    ec_talk:say("A) All", []),
    case ec_talk:ask_default(io_lib:format("[1-~p] or (A)ll ", [length(Apps)]), string, "A") of
        "A" ->
            Apps;
        Index ->
            [lists:nth(list_to_integer(Index), Apps)]
    end.

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
            unicode:characters_to_binary(Term);
        false ->
            [binarify(X) || X <- Term]
    end;
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Term) ->
    Term.

expand_paths(Paths, Dir) ->
    AbsDir = filename:absname(Dir),
    Files = lists:flatmap(fun dir_files1/1, [filename:join(Dir, P) || P <- Paths]),
    [{F1 -- (AbsDir++"/"), F1} || F1 <- filter_regular(Files)].

dir_files1(Dir) ->
    lists:flatmap(fun(Y) -> dir_files(Y) end, filelib:wildcard(Dir)).

filter_regular(Files) ->
    lists:filter(fun filelib:is_regular/1, [filename:absname(F) || F <- Files]).

dir_files(Path) ->
    case filelib:is_dir(Path) of
        true ->
             filelib:wildcard(filename:join(Path, "**"));
        false ->
            [Path]
    end.
