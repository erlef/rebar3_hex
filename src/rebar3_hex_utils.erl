-module(rebar3_hex_utils).

-export([parent_repos/1,
         pretty_print_status/1,
         pretty_print_errors/1,
         format_error/1,
         hex_config_write/1,
         hex_config_read/1,
         repo_opt/0,
         repo/1,
         update_app_src/2,
         select_apps/1,
         binarify/1,
         expand_paths/2,
         get_password/1,
         update_auth_config/2,
         str_split/2,
         get_required/2]).

-include("rebar3_hex.hrl").

pretty_print_status(401) -> "Authentication failed (401)";
pretty_print_status(403) -> "Forbidden (403)";
pretty_print_status(404) -> "Entity not found (404)";
pretty_print_status(422) -> "Validation failed (422)";
pretty_print_status(Code) -> io_lib:format("HTTP status code: ~p", [Code]).

% We wrap rebar_hex_repos:update_auth_config/2 so we can meck it
update_auth_config(Config, State) ->
    rebar_hex_repos:update_auth_config(Config, State).

pretty_print_errors(Errors) ->
    L =  maps:fold(fun(K,V,Acc) ->
                           case is_map(V) of
                               true ->
                                   Acc ++ [pretty_print_errors(V)];
                               false ->
                                   Acc ++ [<<K/binary, " ", V/binary>>]
                           end
                   end,
                   [],
                   Errors),
    binary:list_to_bin(join_lists(", ", L)).

get_required(Key, Args) ->
    case proplists:get_value(Key, Args) of
        undefined ->
            {error, {required, Key}};
        Value ->
            Value
    end.

repo_opt() ->
    {repo, $r, "repo", string, "Repository to use for this command."}.

repo(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    case proplists:get_value(repo, Args, undefined) of
        undefined ->
            DefaultBinName = rebar_utils:to_binary(?DEFAULT_HEX_REPO),
            Res = lists:filter(fun(R) -> maps:get(name, R) =/= DefaultBinName end,
                               Repos),
            case Res of
                [] ->
                    case rebar_hex_repos:get_repo_config(rebar_utils:to_binary(?DEFAULT_HEX_REPO), Repos) of
                        {ok, Repo} ->
                            {ok, Repo};
                        _ ->
                            {error, no_repo_in_state}
                    end;
                [_Repo|_Rest] ->
                    {error, {required, repo}}
            end;
        RepoName ->
            repo(State, RepoName)
    end.

repo(State, RepoName) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    BinName = rebar_utils:to_binary(RepoName),
    MaybeFound1 = get_repo(BinName, Repos),
    MaybeParentRepo = <<"hexpm:">>,
    MaybeFound2 =  get_repo(<<MaybeParentRepo/binary, BinName/binary>>, Repos),
    case {MaybeFound1, MaybeFound2} of
        {{ok, Repo1}, undefined} ->
            {ok, Repo1};
        {undefined, {ok, Repo2}} ->
            {ok, Repo2};
        {undefined, undefined} ->
            {error, {not_valid_repo, RepoName}}
    end.

parent_repos(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    Fun = fun(#{name := Name} = Repo, Acc) ->
                  [Parent|_] = str_split(Name, <<":">>),
                  case maps:is_key(Parent, Acc) of
                    true ->
                        Acc;
                    false ->
                        maps:put(name, Repo, Acc)
                  end
          end,
    Map = lists:foldl(Fun, #{}, Repos),
    {ok, maps:values(Map)}.

get_repo(BinaryName, Repos) ->
    try rebar_hex_repos:get_repo_config(BinaryName, Repos) of
        Name -> Name
    catch
        {error,{rebar_hex_repos,{repo_not_found,BinaryName}}} -> undefined
    end.

hex_config_write(#{write_key := WriteKey, username := Username} = HexConfig) ->
    DecryptedWriteKey = rebar3_hex_user:decrypt_write_key(Username, WriteKey),
    {ok, HexConfig#{api_key => DecryptedWriteKey}};
hex_config_write(_Config) ->
    {error, no_write_key}.

hex_config_read(#{read_key := ReadKey} = HexConfig) ->
    {ok, HexConfig#{api_key => ReadKey}};
hex_config_read(_Config) ->
    {error, no_read_key}.

format_error({not_valid_repo, RepoName}) ->
    io_lib:format("No configuration for repository ~ts found.", [RepoName]).

-define(OP_PUTC, 0).

get_password(Msg) ->
    case os:type() of
        {win32, nt} ->
            get_win32_password(Msg);
        _ ->
            get_tty_password(Msg)
    end.

get_tty_password(Msg) ->
    case io:setopts([binary, {echo, false}]) of
        ok ->
            PwLine = io:get_line(Msg),
            ok = io:setopts([binary, {echo, true}]),
            io:format("\n"),
            [Pw | _] = binary:split(PwLine, <<"\n">>),
            Pw;
        _ ->
            error_logger:tty(false),
            Port = open_port({spawn, "tty_sl -e"}, [binary, eof]),
            port_command(Port, <<?OP_PUTC, Msg/binary>>),
            receive
                {Port, {data, PwLine}} ->
                    [Pw | _] = binary:split(PwLine, <<"\n">>),
                    port_command(Port, <<?OP_PUTC, $\n>>),
                    port_close(Port),
                    error_logger:tty(true),
                    Pw
            end
    end.

get_win32_password(Prompt) ->
    ok = io:setopts([binary]),
    Overwriter = fun() ->
        prompt_win32_password(Prompt),
        receive
            {done, _Pid, _Ref} ->
                ok
        end
    end,
    Pid = spawn_link(Overwriter),
    PwLine = try
        io:get_line(Prompt)
    after
        Ref = make_ref(),
        Pid ! {done, self(), Ref},
        receive
            {done, Pid, Ref} ->
                ok
        after
            timer:seconds(5) ->
                throw(?PRV_ERROR(win32_prompt_timeout))
        end
    end,
    [Pw | _] = binary:split(PwLine, <<"\n">>),
    Pw.

prompt_win32_password(Prompt) ->
    % This is spawned to continually overwrite the prompt the user is
    % entering data on, in order to hide characters typed.
    ClearLine = "\e[2K",
    receive
        {done, Parent, Ref} ->
            Parent ! {done, self(), Ref},
            Spaces = lists:duplicate(byte_size(Prompt) + 24, $ ),
            io:fwrite(standard_error, "~ts\r~ts\r", [ClearLine, Spaces])
    after
        1 ->
            Spaces = lists:duplicate(24, $ ),
            io:fwrite(standard_error, "~ts\r~ts~ts\r~ts", [ClearLine, Prompt, Spaces, Prompt]),
            prompt_win32_password(Prompt)
    end.

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

-ifdef(POST_OTP_18).
join_lists(Sep, List) -> lists:join(Sep, List).
-else.
join_lists(_Sep, []) -> [];
join_lists(Sep, List) ->
    [Last | AllButLast] = lists:reverse(List),
    lists:foldl(fun (Elem,Acc) -> [Elem,Sep|Acc] end, [Last], AllButLast).
-endif.

-ifdef(POST_OTP_19).
str_split(Str, Pattern) ->
    string:split(Str, Pattern).
-else.
str_split(Str, Pattern) ->
    Bin = unicode:characters_to_binary(Str),
    Bpat = unicode:characters_to_binary(Pattern),
    Blist = binary:split(Bin, Bpat),
    lists:map(fun(B) -> unicode:characters_to_list(B) end, Blist).
-endif.

