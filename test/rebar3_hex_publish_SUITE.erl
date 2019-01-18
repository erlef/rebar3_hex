-module(rebar3_hex_publish_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [  publish_invalid_semver
     , publish_no_descrip
     , publish_empty_descrip
     , publish_has_contributors
     , publish_has_maintainers
     , publish_no_licenses
     , publish_empty_licenses
     , publish_multi_errors
    ].

publish_invalid_semver(Config) ->
    App = mock_app("bad_ver", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp = {error,
            {rebar3_hex_publish,
             [{invalid_semver,<<"bad_ver">>,"0.a.1b..0.-foo"}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_has_contributors(Config) ->
    App = mock_app("has_contributors", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp =  {error,
                {rebar3_hex_publish,
                    [{has_contributors,<<"has_contributors">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_has_maintainers(Config) ->
    App = mock_app("has_maintainers", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp = {error,{rebar3_hex_publish,[{has_maintainers,<<"has_maintainers">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_no_descrip(Config) ->
    App = mock_app("no_descrip", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp = {error,{rebar3_hex_publish,[{no_description,<<"no_descrip">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_no_licenses(Config) ->
    App = mock_app("no_licenses", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp = {error,{rebar3_hex_publish,[{no_license,<<"no_licenses">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_empty_licenses(Config) ->
    App = mock_app("empty_licenses", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp = {error,{rebar3_hex_publish,[{no_license, <<"empty_licenses">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_empty_descrip(Config) ->
    App = mock_app("empty_descrip", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp = {error,{rebar3_hex_publish,[{no_description,<<"empty_descrip">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_multi_errors(Config) ->
    App = mock_app("multi_errors", Config),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    State = rebar_state:new(),
    Exp = {error,
         {rebar3_hex_publish,
             [{invalid_semver,<<"multi_errors">>,"0.1b-prod"},
              {has_maintainers,<<"multi_errors">>},
              {no_description,<<"multi_errors">>},
              {no_license,<<"multi_errors">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

mock_app(AppName, Config) ->
    Src = filename:join([?config(data_dir, Config), "test_apps/" ++ AppName]),
    {ok, App} = rebar_app_info:discover(Src),
    App.
