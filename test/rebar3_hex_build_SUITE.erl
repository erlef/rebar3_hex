-module(rebar3_hex_build_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        create_package_test,
        create_package_with_git_deps_test,
        create_package_no_lock_file,
        create_package_with_alias_deps,
        create_package_with_binary_versions,
        create_docs_with_no_repo_set,
        create_docs_test,
        create_docs_unknown_provider_test,
        create_docs_no_provider_test,
        create_docs_doc_dir_test,
        create_docs_doc_dir_missing_test,
        create_docs_provider_failure_test
    ].

create_package_test(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "valid"},
    {State, _Repo, App} = test_utils:make_stub(StubConfig),
    {ok, #{tarball := Tarball} = Package} = rebar3_hex_build:create_package(State, App),
    ?assert(maps:is_key(tarball, Package)),
    ?assert(maps:is_key(version, Package)),
    ?assert(maps:is_key(files, Package)),
    ?assert(maps:is_key(has_checkouts, Package)),
    ?assert(maps:is_key(metadata, Package)),
    {ok, Files} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    ContentTar = proplists:get_value("contents.tar.gz", Files),
    {ok, Files1} = hex_erl_tar:extract({binary, ContentTar}, [memory, compressed]),
    #{files := PkgFiles} = Package,
    ExpFiles = lists:foldl(fun({N, P}, Acc) ->
                                case N of
                                   "src/valid.app.src" ->
                                         [{N, P}|Acc];
                                    _ ->
                                        case file:read_file(P) of
                                            {ok, Bin} ->
                                                [{N, Bin}|Acc];
                                            _ ->
                                                Acc
                                        end
                                end
                                        end, [], PkgFiles),
    SortedExp = lists:sort(ExpFiles),
    SortedFiles = lists:sort(Files1),
    ?assertMatch(SortedExp, SortedFiles).

create_package_with_git_deps_test(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "valid", profile => with_git_deps},
    {State, _Repo, App} = test_utils:make_stub(StubConfig),
    {error, _} = rebar3_hex_build:create_package(State, App).

create_package_no_lock_file(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "no_lock_file", profile => no_lock_file},
    {State, _Repo, App} = test_utils:make_stub(StubConfig),
    ?assertError({error, {rebar3_hex_build, {create_package, no_lock_file}}}, rebar3_hex_build:create_package(State, App)).

create_package_with_binary_versions(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "valid", profile => with_binary_versions},
    {State, _Repo, App} = test_utils:make_stub(StubConfig),
    {ok, _} = rebar3_hex_build:create_package(State, App).

create_package_with_alias_deps(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "valid", profile => with_alias_deps},
    {State, _Repo, App} = test_utils:make_stub(StubConfig),
    {ok, _} = rebar3_hex_build:create_package(State, App).

create_docs_test(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "valid"},
    {State, Repo, App} = test_utils:make_stub(StubConfig),
    {ok, #{tarball := Tarball}} = rebar3_hex_build:create_docs(State, Repo, App),
    {ok, Files} = hex_erl_tar:extract({binary, Tarball}, [memory, compressed]),
    Found = [ X || {X, _} <- Files],
    Exp = ["edoc-info",
           "erlang.png",
           "index.html",
           "modules-frame.html",
           "overview-summary.html",
           "stylesheet.css",
           "valid.html"
          ],
    ?assertMatch(Exp, Found).

create_docs_unknown_provider_test(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "valid"},
    {State, Repo, App} = test_utils:make_stub(StubConfig),
    Repo1 = Repo#{doc => #{provider => foo}},
    ?assertMatch({error, {doc_provider_not_found, foo}}, rebar3_hex_build:create_docs(State, Repo1, App)).

create_docs_no_provider_test(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "no_doc_config"},
    {State, Repo, App} = test_utils:make_stub(StubConfig),
    Repo1 = maps:remove(doc, Repo),
    State1 = rebar_state:set(State, hex, [{repos, [Repo]}]),
    ?assertMatch({error, no_doc_config}, rebar3_hex_build:create_docs(State1, Repo1, App)).

create_docs_with_no_repo_set(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "no_repo_set"},
    {State, _Repo, App} = test_utils:make_stub(StubConfig),
    State1 = rebar_state:set(State, hex, [{doc, edoc}]),
    ?assertMatch({ok, _}, rebar3_hex_build:create_docs(State1, #{name => <<"some_repo">>}, App)).

create_docs_doc_dir_test(Config) ->
    #{dir := RootDir} = StubConfig = #{type => app, dir => data_dir(Config), name => "doc_attr"},
    {State, Repo, App} = test_utils:make_stub(StubConfig),
    DocDir = filename:join([RootDir, "doc_attr", "doc"]),
    test_utils:mkdir_p(DocDir),
    ok = file:write_file(filename:join([DocDir, "index.html"]), "eh?"),
    Repo1 = Repo#{doc => #{}},
    ?assertMatch({ok, _Docs}, rebar3_hex_build:create_docs(State, Repo1, App, #{doc_dir => "doc"})).

create_docs_doc_dir_missing_test(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "doc_attr"},
    {State, Repo, App} = test_utils:make_stub(StubConfig),
    Repo1 = Repo#{doc => #{}},
    ?assertMatch({ok, _Docs}, rebar3_hex_build:create_docs(State, Repo1, App, #{doc_dir => "doc"})).

create_docs_provider_failure_test(Config) ->
    StubConfig = #{type => app, dir => data_dir(Config), name => "valid"},
    {State, Repo, App} = test_utils:make_stub(StubConfig),
    Repo1 = Repo#{doc => #{provider => bad_doc}},
    {ok, State1} = bad_doc_provider:init(State),
    ?assertMatch({error, {doc_provider_failed, bad_doc}}, rebar3_hex_build:create_docs(State1, Repo1, App)).

data_dir(Config) -> ?config(priv_dir, Config).
