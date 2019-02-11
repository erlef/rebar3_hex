-module(test_utils).

-export([mock_app/2, mock_command/2, mock_command/3, repo_config/0, repo_config/1]).

-define(REPO_CONFIG, maps:merge(hex_core:default_config(), #{
                                  name        => <<"foo">>,
                                  repo        => <<"foo">>,
                                  api_url     => <<"http://127.0.0.1:3000">>,
                                  repo_url    => <<"http://127.0.0.1:3000">>,
                                  repo_verify => false,
                                  read_key                 => <<"123">>,
                                  repo_public_key          => <<0>>,
                                  repos_key                => <<"repos_key">>,
                                  username                 => <<"mr_pockets">>,

                                  write_key               => rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>,
                                  <<"special_shoes">>, <<"key">>)
                                 })).

mock_app(AppName, DataDir) ->
    Src = filename:join([DataDir, "test_apps/" ++ AppName]),
    {ok, App} = rebar_app_info:discover(Src),
    State = rebar_state:project_apps(rebar_state(repo_config()), [App]),
    {ok, App, State}.

mock_command(Command, Repo) ->
    mock_command(Command, Repo, rebar_state(Repo)).

mock_command(Command, Repo, State0) ->
    State1 = rebar_state:add_resource(State0, {pkg, rebar_pkg_resource}),
    State2 = rebar_state:create_resources([{pkg, rebar_pkg_resource}], State1),
    rebar_state:command_args(State2, [Command]).

rebar_state(Repo) ->
    rebar_state:new([{command_parsed_args, []}, {resources, []}, {hex, [{repos, [Repo]}]}]).

repo_config() ->
    ?REPO_CONFIG.
repo_config(Cfg) ->
    maps:merge(?REPO_CONFIG, Cfg).

