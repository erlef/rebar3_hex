-module(rebar3_hex_config_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [repo, api_key_name_test, encrypt_decrypt_write_key_test, org_key_name_test].

api_key_name_test(_Config) ->
    ?assertEqual(<<"foo-api">>, rebar3_hex_config:api_key_name(<<"foo">>)),
    ?assertEqual(<<"foo-api-org">>, rebar3_hex_config:api_key_name(<<"foo">>, <<"org">>)).

encrypt_decrypt_write_key_test(_Config) ->
    WriteKey = <<"abc1234">>,

    Username = <<"user">>,
    LocalPassword = <<"password">>,

    WriteKeyEncrypted = rebar3_hex_user:encrypt_write_key(Username, LocalPassword, WriteKey),

    ?assertMatch(error,
                  rebar3_hex_config:decrypt_write_key(Username, <<"wrong password">>, WriteKeyEncrypted)),

    ?assertEqual(WriteKey, rebar3_hex_config:decrypt_write_key(Username, LocalPassword, WriteKeyEncrypted)).

org_key_name_test(_Config) ->
    {ok, Name} = inet:gethostname(),
    BinName = list_to_binary(Name),
    Exp = <<BinName/binary, "-repository-org">>,
    ?assertEqual(Exp, rebar3_hex_config:org_key_name(undefined, <<"org">>)),
    ?assertEqual(<<"foo-repository-org">>, rebar3_hex_config:org_key_name(<<"foo">>, <<"org">>)).

repo(_Config) ->
    ?assertError(function_clause, rebar3_hex_config:repo({})),
    ?assertError({badmatch, {error, not_found}}, rebar3_hex_config:repo(rebar_state:new())),
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
    _Resource = rebar_resource_v2:new(pkg, rebar_pkg_resource, PkgResConfig),
    State0 = rebar_state:new([{resources, []},
                             {command_parsed_args, []}]),
    State1 = rebar_state:add_resource(State0, {pkg, rebar_pkg_resource}),
    {ok, Repo} = rebar3_hex_config:repo(State1),
    true = is_map(Repo),
    State2 = rebar_state:new([{resources, []},
                             {command_parsed_args, {[{repo,"eh"}],[]}}
                             ]),
    State3 = rebar_state:command_parsed_args(State2, {[{repo,"eh"}],[]}),
    _Resource1 = rebar_resource_v2:new(pkg, rebar_pkg_resource, PkgResConfig),
    State4 = rebar_state:add_resource(State3, {pkg, rebar_pkg_resource}),
    ?assertMatch({error,{not_valid_repo,"eh"}}, rebar3_hex_config:repo(State4)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

mock_app(AppName, Config) ->
    Src = filename:join([?config(data_dir, Config), "test_apps/" ++ AppName]),
    {ok, App} = rebar_app_info:discover(Src),
    App.
