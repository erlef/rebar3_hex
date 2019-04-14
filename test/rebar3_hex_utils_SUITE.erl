-module(rebar3_hex_utils_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [pretty_print_status,
     pretty_print_errors,
    repo_opt, repo, format_error_test, binarify_test, expand_paths_test].

pretty_print_errors(_Config) ->
    Errors = #{<<"emails">>=>#{<<"email">>=><<"already in use">>}},
    ?assertEqual(<<"email already in use">>, rebar3_hex_utils:pretty_print_errors(Errors)).

pretty_print_status(_Config) ->
    ?assertEqual("Authentication failed (401)", rebar3_hex_utils:pretty_print_status(401)),
    ?assertEqual("Forbidden (403)", rebar3_hex_utils:pretty_print_status(403)),
    ?assertEqual("Entity not found (404)",
                 rebar3_hex_utils:pretty_print_status(404)),
    ?assertEqual("Validation failed (422)",
                 rebar3_hex_utils:pretty_print_status(422)),
    ?assertEqual(lists:append("HTTP status code: ", ["418"]),
                 rebar3_hex_utils:pretty_print_status(418)).

repo_opt(_Config) ->
    ?assertEqual({repo,114,"repo",string,
      "Repository to use for this command."}, rebar3_hex_utils:repo_opt()).

repo(_Config) ->
    ?assertError(function_clause, rebar3_hex_utils:repo({})),
    ?assertError({badmatch, {error, not_found}}, rebar3_hex_utils:repo(rebar_state:new())),
    Agent = <<"(rebar3/3.7.5+build.4236.ref5ff24a8) (httpc)">>,
    PkgResConfig = #{
                        base_config => #{
                            http_adapter=> hex_http_httpc,
                            http_adapter_config=> #{profile => rebar},
                            http_user_agent_fragment => Agent
                        },
                        repos => [
                                #{
                                    api_url                  => <<"https://hex.pm/api">>,
                                    http_adapter             => hex_http_httpc,
                                    http_adapter_config      =>#{profile=>rebar},
                                    http_user_agent_fragment => Agent,
                                    name                     => <<"hexpm">>,
                                    read_key                 => <<"0">>,
                                    repo_public_key          => <<0>>,
                                    repo_url                 => <<"https://repo.hex.pm">>,
                                    repo_verify              => true,
                                    repo_key                => <<"key">>,
                                    username                 => <<"starbelly">>,
                                    write_key               => {<<0>>,{<<0>>}}
                                }
                    ]
                },
    State0 = rebar_state:new([{resources, []},
                             {command_parsed_args, []}]),
    _Resource = rebar_resource_v2:new(pkg, rebar_pkg_resource, PkgResConfig),
    State1 = rebar_state:add_resource(State0, {pkg, rebar_pkg_resource}),
    Repo = rebar3_hex_utils:repo(State1),
    true = is_map(Repo),
    State2 = rebar_state:new([{resources, []},
                             {command_parsed_args, {[{repo,"eh"}],[]}}
                             ]),
    State3 = rebar_state:command_parsed_args(State2, {[{repo,"eh"}],[]}),
    _Resource1 = rebar_resource_v2:new(pkg, rebar_pkg_resource, PkgResConfig),
    State4 = rebar_state:add_resource(State3, {pkg, rebar_pkg_resource}),
    ?assertThrow({error, {rebar_hex_repos, {repo_not_found, <<"eh">>}}}, rebar3_hex_utils:repo(State4)).

format_error_test(_Config) ->
    Exp = <<"No configuration for repository foo found.">>,
    ?assertEqual(Exp, iolist_to_binary(rebar3_hex_utils:format_error({not_valid_repo, foo}))).

binarify_test(_Config) ->
    ?assertEqual(true, rebar3_hex_utils:binarify(true)),
    ?assertEqual(<<"eh">>, rebar3_hex_utils:binarify(eh)),
    ?assertEqual([], rebar3_hex_utils:binarify([])),
    ?assertEqual([<<"eh">>, <<"foo">>], rebar3_hex_utils:binarify(["eh", foo])),
    ?assertMatch(#{<<"eh">> := <<"eh">>,<<"foo">> := <<"foo">>},
    rebar3_hex_utils:binarify(#{eh => "eh", foo => foo})),
    ?assertEqual(2.3, rebar3_hex_utils:binarify(2.3)).

expand_paths_test(_Config) ->
    ?assertEqual([], rebar3_hex_utils:expand_paths(["/foo"], "src")),
    {ok, Dir} = file:get_cwd(),
    Dir2 = string:join([Dir,  "foo"], "/"),
    ok = file:make_dir(Dir2),
    File = string:join([Dir2, "bar"], "/"),
    ok = file:write_file(File, ""),
    ?assertEqual([{"bar", File}], rebar3_hex_utils:expand_paths(["bar"], "foo")),
    Exp = [{"foo/bar", File}],
    ?assertEqual(Exp, rebar3_hex_utils:expand_paths(["foo*"], Dir)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

mock_app(AppName, Config) ->
    Src = filename:join([?config(data_dir, Config), "test_apps/" ++ AppName]),
    {ok, App} = rebar_app_info:discover(Src),
    App.
