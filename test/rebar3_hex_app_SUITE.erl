-module(rebar3_hex_app_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [get_deps_test, validate_test].

get_deps_test(_Config) ->
    Deps =
        [
            {<<"verl">>,
                {pkg, <<"verl">>, <<"1.1.1">>,
                    <<"98F3EC48B943AA4AE8E29742DE86A7CD752513687911FE07D2E00ECDF3107E45">>,
                    <<"0925E51CD92A0A8BE271765B02430B2E2CFF8AC30EF24D123BD0D58511E8FB18">>},
                0}
        ],

    Exp =
        {ok, [
            {<<"verl">>, [
                {<<"app">>, <<"verl">>},
                {<<"optional">>, false},
                {<<"requirement">>, <<"1.1.1">>}
            ]}
        ]},

    ?assertMatch(Exp, rebar3_hex_app:get_deps(Deps)),

    NoTopLevelDeps =
        [
            {<<"hex_core">>,
                {pkg, <<"hex_core">>, <<"0.8.2">>,
                    <<"48D52F273A54F3E1564E9D4E89731EB4C47629A8C0FC03B88C4098B75AEA55CC">>,
                    <<"D5E92BD919FC9872FCBCCA1CD13FF094F7BF520EAA29C9D14B674D16A2E1A089">>},
                1},
            {<<"verl">>,
                {pkg, <<"verl">>, <<"1.1.1">>,
                    <<"98F3EC48B943AA4AE8E29742DE86A7CD752513687911FE07D2E00ECDF3107E45">>,
                    <<"0925E51CD92A0A8BE271765B02430B2E2CFF8AC30EF24D123BD0D58511E8FB18">>},
                1}
        ],

    ?assertMatch({ok, []}, rebar3_hex_app:get_deps(NoTopLevelDeps)),

    ForbiddenDeps =
        [
            {<<"verl">>,
                {git, "https://github.com/jelly-beam/verl",
                    {ref, "554c98ad7be8983f831c259f269b7a66e2690c12"}},
                0}
        ],

    ?assertMatch({error, {non_hex_deps, ["verl"]}}, rebar3_hex_app:get_deps(ForbiddenDeps)).

validate_test(_Config) ->
    Exp = {error, #{errors => [{invalid_semver, {some_app, "0.a.1b..0.-foo"}}], warnings => []}},
    AppData = #{
        name => some_app,
        version => "0.a.1b..0.-foo",
        details => app_details(bad_semver),
        deps => []
    },
    ?assertMatch(
        Exp, rebar3_hex_app:validate(AppData)
    ),

    Exp1 = {error, #{errors => [], warnings => [{no_description, some_app}]}},
    AppData1 = #{
        name => some_app, version => "1.1.1", details => app_details(no_descrip), deps => []
    },
    ?assertMatch(Exp1, rebar3_hex_app:validate(AppData1)),

    Exp2 = {error, #{errors => [], warnings => [{no_license, some_app}]}},
    AppData2 = #{
        name => some_app, version => "1.1.1", details => app_details(no_licenses), deps => []
    },
    ?assertMatch(Exp2, rebar3_hex_app:validate(AppData2)),

    Exp3 = {error, #{errors => [], warnings => [{no_license, some_app}]}},
    AppData3 = #{
        name => some_app, version => "1.1.1", details => app_details(empty_licenses), deps => []
    },
    ?assertMatch(Exp3, rebar3_hex_app:validate(AppData3)),

    Exp4 = {error, #{errors => [], warnings => [{no_description, some_app}]}},
    AppData4 = #{
        name => some_app, version => "1.1.1", details => app_details(empty_descrip), deps => []
    },
    ?assertMatch(Exp4, rebar3_hex_app:validate(AppData4)),

    Exp5 =
        {error, #{
            errors => [{invalid_semver, {some_app, "0.a.1b..0.-foo"}}],
            warnings =>
                [{no_license, some_app}, {no_description, some_app}]
        }},
    AppData5 = #{
        name => some_app,
        version => "0.a.1b..0.-foo",
        details => app_details(multi_errors),
        deps => []
    },
    ?assertMatch(
        Exp5, rebar3_hex_app:validate(AppData5)
    ),
    ok.

app_details(bad_semver) ->
    [{vsn, "0.a.1b..0.-foo"} | proplists:delete(vsn, base_app_details())];
app_details(no_descrip) ->
    proplists:delete(description, base_app_details());
app_details(no_licenses) ->
    proplists:delete(licenses, base_app_details());
app_details(empty_descrip) ->
    [{description, ""} | proplists:delete(description, base_app_details())];
app_details(empty_licenses) ->
    [{licenses, []} | proplists:delete(licenses, base_app_details())];
app_details(multi_errors) ->
    Props = app_details(bad_semver),
    Props1 = proplists:delete(description, Props),
    proplists:delete(licenses, Props1).

base_app_details() ->
    [
        {description, "An OTP pplication"},
        {vsn, "1.1.1"},
        {registered, []},
        {mod, {some_app, []}},
        {applications, [kernel, stdlib]},
        {env, []},
        {modules, []},
        {licenses, ["Apache 2.0"]},
        {links, []}
    ].
