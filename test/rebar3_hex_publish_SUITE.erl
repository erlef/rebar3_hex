-module(rebar3_hex_publish_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [  publish_invalid_semver_test
     , publish_no_descrip_test
     , publish_empty_descrip_test
     , publish_has_contributors_test
     , publish_has_maintainers_test
     , publish_no_licenses_test
     , publish_empty_licenses_test
     , publish_multi_errors_test
     , format_error_test
    ].

publish_invalid_semver_test(Config) ->
    {ok, App, State} = test_utils:mock_app("bad_ver", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp = {error,
            {rebar3_hex_publish,
             [{invalid_semver,<<"bad_ver">>,"0.a.1b..0.-foo"}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_has_contributors_test(Config) ->
    {ok, App, State} = test_utils:mock_app("has_contributors", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp =  {error,
                {rebar3_hex_publish,
                    [{has_contributors,<<"has_contributors">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_has_maintainers_test(Config) ->
    {ok, App, State} = test_utils:mock_app("has_maintainers", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp = {error,{rebar3_hex_publish,[{has_maintainers,<<"has_maintainers">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_no_descrip_test(Config) ->
    {ok, App, State} = test_utils:mock_app("no_descrip", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp = {error,{rebar3_hex_publish,[{no_description,<<"no_descrip">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_no_licenses_test(Config) ->
    {ok, App, State} = test_utils:mock_app("no_licenses", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp = {error,{rebar3_hex_publish,[{no_license,<<"no_licenses">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_empty_licenses_test(Config) ->
    {ok, App, State} = test_utils:mock_app("empty_licenses", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp = {error,{rebar3_hex_publish,[{no_license, <<"empty_licenses">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_empty_descrip_test(Config) ->
    {ok, App, State} = test_utils:mock_app("empty_descrip", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp = {error,{rebar3_hex_publish,[{no_description,<<"empty_descrip">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

publish_multi_errors_test(Config) ->
    {ok, App, State} = test_utils:mock_app("multi_errors", ?config(data_dir, Config)),
    Repo = maps:merge(hex_core:default_config(),
			#{name => <<"foo">>, repo => <<"foo">>,
			  api_url => <<"http://127.0.0.1:3000">>,
			  repo_url => <<"http://127.0.0.1:3000">>,
			  repo_verify => false}),
    Exp = {error,
         {rebar3_hex_publish,
             [{invalid_semver,<<"multi_errors">>,"0.1b-prod"},
              {has_maintainers,<<"multi_errors">>},
              {no_description,<<"multi_errors">>},
              {no_license,<<"multi_errors">>}]}},
    ?assertEqual(Exp, rebar3_hex_publish:publish(App, Repo, State)),
    ok.

format_error_test(_Config) ->
    ReqTimeErrs = [<<"Status Code: Validation failed (422)\nHex Error: your app is bad!\n\t">>,
    <<"Inserted At: can only modify a release up to one hour after creation\n">>,
    <<"Requirements could not be computed\nnil\n--------------------\nno bueno\n">>],

    ErrMap = [
              {{invalid_semver, myapp, "-1.2.3"},
               <<"myapp.app.src : non-semantic version number \"-1.2.3\" found">>},
              {{no_description, myapp},
               <<"myapp.app.src : missing or empty description property">>},
              {{no_license, myapp},
               <<"myapp.app.src : missing or empty licenses property">>},
              {{has_maintainers, myapp},
               <<"myapp.app.src : deprecated field maintainers found">>},
              {{has_contributors, myapp},
               <<"myapp.app.src : deprecated field contributors found">>},
              {no_write_key,
               <<"No write key found for user. Be sure to authenticate first with: rebar3 hex user auth">>},
              {{validation_errors, [<<"Bad things">>, {<<"More bad">>, <<"things">>}], <<"eh?">>},
               <<"Failed to publish package: eh?\n\tBad thingsMore bad: things">>
              },
              {{publish_failed, "non sequitur"}, <<"Failed to publish package: non sequitur">>},
              {{non_hex_deps, ["dep1", "dep2", "dep3"]},
               <<"Can not publish package because the following deps are not available in hex: dep1, dep2, dep3">>},
              {undefined_server_error, <<"Unknown server error">>},
              {{status, 403}, <<"Forbidden (403)">>},
              {{status, 500, undefined_server_error}, <<"Unknown server error: HTTP status code: 500">>},
              {{status, 422,
                #{
                  <<"message">> => <<"your app is bad!">>,
                  <<"errors">> => #{ <<"requirements">> => #{nil => <<"no bueno">>},
                                     <<"inserted_at">> => <<"can only modify a release up to one hour after creation">>
                                   }}},
               list_to_binary(ReqTimeErrs)
              }
             ],

    ErrList = lists:map(fun({Args, _Exp}) ->  Args end, ErrMap),

    BigStr = lists:foldl(fun({_Args, Exp}, Acc) ->
                                 <<Acc/binary, Exp/binary, <<"\n     ">>/binary>>
                         end,
                         <<"Validator Errors:\n     ">>,
                         ErrMap),

    lists:foreach(fun({Args, Exp}) ->
                          ?assertEqual(Exp, list_to_bitstring(rebar3_hex_publish:format_error(Args)))
                  end, ErrMap),

    TrimStr = re:replace(BigStr, "^\\s+|\\s+$", "", [{return, binary}, global]),

    Info = <<"Please see https://hex.pm/docs/rebar3_publish for more info.\n">>,

    ?assertEqual(<<TrimStr/binary, <<"\n\n\n     ">>/binary, Info/binary>>,
                 list_to_bitstring(rebar3_hex_publish:format_error(ErrList))).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
