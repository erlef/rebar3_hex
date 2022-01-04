-module(rebar3_hex_file_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [expand_paths_test, expand_once_test].

expand_paths_test(_Config) ->
    ?assertEqual([], rebar3_hex_file:expand_paths(["/foo"], "src")),
    {ok, Dir} = file:get_cwd(),
    Dir2 = string:join([Dir,  "foo"], "/"),
    ok = file:make_dir(Dir2),
    File = string:join([Dir2, "bar"], "/"),
    ok = file:write_file(File, ""),
    ?assertEqual([{"bar", File}], rebar3_hex_file:expand_paths(["bar"], "foo")),
    Exp = [{"foo", Dir2}, {"foo/bar", File}],
    ?assertEqual(Exp, rebar3_hex_file:expand_paths(["foo*"], Dir)).

expand_once_test(_Config) ->
    {ok, Dir} = file:get_cwd(),
    Dir2 = filename:join([Dir,  "ouch", "regression", "foo", "bar"]),
    ok = test_utils:mkdir_p(Dir2),
    file:set_cwd(Dir2),
    ok = file:make_symlink("../../../", "baz"),
    Exp = [{"ouch", filename:join(Dir, "ouch")}, 
           {"ouch/regression", filename:join(Dir, "ouch/regression")},
           {"ouch/regression/foo", filename:join(Dir, "ouch/regression/foo")},
           {"ouch/regression/foo/bar", filename:join(Dir, "ouch/regression/foo/bar")},
           {"ouch/regression/foo/bar/baz", filename:join(Dir, "ouch/regression/foo/bar/baz")}
          ],
    ?assertMatch(Exp, rebar3_hex_file:expand_paths(["ouch"], Dir)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

mock_app(AppName, Config) ->
    Src = filename:join([?config(data_dir, Config), "test_apps/" ++ AppName]),
    {ok, App} = rebar_app_info:discover(Src),
    App.
