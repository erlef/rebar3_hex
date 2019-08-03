-module(rebar3_hex_file_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [expand_paths_test].

expand_paths_test(_Config) ->
    ?assertEqual([], rebar3_hex_file:expand_paths(["/foo"], "src")),
    {ok, Dir} = file:get_cwd(),
    Dir2 = string:join([Dir,  "foo"], "/"),
    ok = file:make_dir(Dir2),
    File = string:join([Dir2, "bar"], "/"),
    ok = file:write_file(File, ""),
    ?assertEqual([{"bar", File}], rebar3_hex_file:expand_paths(["bar"], "foo")),
    Exp = [{"foo/bar", File}],
    ?assertEqual(Exp, rebar3_hex_file:expand_paths(["foo*"], Dir)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

mock_app(AppName, Config) ->
    Src = filename:join([?config(data_dir, Config), "test_apps/" ++ AppName]),
    {ok, App} = rebar_app_info:discover(Src),
    App.
