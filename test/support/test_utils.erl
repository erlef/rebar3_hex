-module(test_utils).

-export([default_config/0, mkdir_p/1, make_stub/1, stub_app/1, mock_command/4, repo_config/0, repo_config/1]).

-define(HEXPM_PUBLIC_KEY, <<"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
0wIDAQAB
-----END PUBLIC KEY-----">>).

-define(REPO_CONFIG, maps:merge(hex_core:default_config(), #{
                                  name        => <<"hexpm">>,
                                  repo        => <<"hexpm">>,
                                  api_url     => <<"http://127.0.0.1:3000">>,
                                  repo_url    => <<"http://127.0.0.1:3000">>,
                                  repo_verify => false,
                                  read_key                 => <<"123">>,
                                  repo_public_key          => ?HEXPM_PUBLIC_KEY,
                                  repo_key                => <<"repo_key">>,
                                  username                 => <<"mr_pockets">>,
                                  write_key               => rebar3_hex_user:encrypt_write_key(<<"mr_pockets">>,
                                  <<"special_shoes">>, <<"key">>),
                                  doc => #{provider => edoc}
                                 }
                               )).


default_config() -> ?REPO_CONFIG.

mock_command(ProviderName, Command, RepoConfig, State0) ->
    State1 = rebar_state:add_resource(State0, {pkg, rebar_pkg_resource}),
    State2 = rebar_state:create_resources([{pkg, rebar_pkg_resource}], State1),
    State3 = rebar_state:set(State2, hex, RepoConfig),
    State4 = rebar_state:command_args(State3, Command),
    {ok, State5} = ProviderName:init(State4),

    Provider = providers:get_provider_by_module(ProviderName, rebar_state:providers(State5)),

    {ok, State6} = rebar_prv_edoc:init(State5),

    Opts = providers:opts(Provider) ++ rebar3:global_option_spec_list(),
    {ok, Args} = getopt:parse(Opts, rebar_state:command_args(State6)),
    {ok, rebar_state:command_parsed_args(State6, Args)}.

make_stub(#{type := app, name := Name, dir := Dir} = StubConfig) ->
    AppDir = filename:join(Dir, [Name]),
    mkdir_p(AppDir),
    StubConfig1 = put_defaults(StubConfig),
    _SrcFile = write_src_file(AppDir, StubConfig1),
    _AppSrcFile = write_app_src_file(AppDir, StubConfig1),
    _ConfigFile = write_config_file(AppDir, StubConfig1),
    _LockFile = write_lock_file(AppDir, StubConfig1),
    #{repo := Repo} = StubConfig1,
    State = init_state(AppDir, Repo, StubConfig1),
    {ok, App} = rebar3_hex_app:find(rebar_state:project_apps(State), Name),
    {ok, State1} = rebar_prv_edoc:init(State),
    {State1, Repo, App};

make_stub(#{type := umbrella, dir := Dir, name := Name, apps := Apps} = StubConfig) ->
    _ProjectDir = filename:join(Dir, [Name]),
    StubConfig1 = put_defaults(StubConfig),
    #{repo := Repo} = StubConfig1,
    RootDir = filename:join(Dir, [Name]),
    make_stub(StubConfig#{type => app, dir => Dir}),
    lists:foreach(fun(#{name := _AppName} = App) -> 
            AppDir = filename:join(RootDir, ["apps/"]),
            make_stub(App#{type => app, dir => AppDir})
        end, Apps),
    State = init_state(RootDir, Repo, StubConfig1),
    {ok, App} = rebar3_hex_app:find(rebar_state:project_apps(State), Name),
    {ok, State1} = rebar_prv_edoc:init(State),
    {State1, Repo, App}.

init_state(Dir, Repo, #{profile := Profile}) ->
    State = rebar_state(Dir, Repo, locks(Profile)),
    LibDirs = rebar_dir:lib_dirs(State),
    rebar_app_discover:do(State, LibDirs).

put_defaults(Cfg) -> 
    Cfg1 = maybe_put_default_repo(Cfg),
    Cfg2 = maybe_put_default_profile(Cfg1),
    maybe_put_default_app_src_spec(Cfg2).

maybe_put_default_repo(#{repo := _} = Cfg) -> Cfg;
maybe_put_default_repo(Cfg) -> Cfg#{repo => test_utils:repo_config(#{doc => #{provider => edoc}})}.

maybe_put_default_app_src_spec(#{app_src := _} = Cfg) -> Cfg;
maybe_put_default_app_src_spec(Cfg) -> Cfg#{app_src => #{version => "0.1.0"}}.

stub_app(#{dir := Dir, name := AppName} = StubConfig) ->
    StubConfig1 = maybe_put_default_profile(StubConfig),
    AppsDir = filename:join([Dir, AppName]),
    ok = mkdir_p(AppsDir),
    SrcFile = write_src_file(AppsDir, StubConfig1),
    AppSrcFile = write_app_src_file(AppsDir, StubConfig1),
    ConfigFile = write_config_file(AppsDir, StubConfig1),
    LockFile = write_lock_file(AppsDir, StubConfig1),
    #{dir => Dir,
      lock_file => LockFile,
      src_file => SrcFile,
      app_src_file => AppSrcFile,
      config_file => ConfigFile
     }.

maybe_put_default_profile(#{profile := _Profile} = Cfg) -> Cfg;
maybe_put_default_profile(Cfg) -> Cfg#{profile => default}.

write_src_file(Dir, #{name := Name}) ->
    Erl = filename:join([Dir, "src/", Name ++ ".erl"]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, erl_src_file(Name)).

write_app_src_file(Dir, #{name := Name, app_src := #{version := Vsn}}) ->
    Filename = filename:join([Dir, "src", Name ++ ".app.src"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(Name, Vsn)).

write_lock_file(Dir, #{profile := Profile}) ->
    Filename = filename:join([Dir, "rebar.lock"]),
    ok = filelib:ensure_dir(Filename),
    ok = rebar_config:write_lock_file(Filename, locks(Profile)),
    Filename.

write_config_file(Dir, #{profile := Profile}) ->
    Filename = filename:join([Dir, "rebar.config"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, config(Profile)).

config(default) ->
    {deps, [{hex_core, "0.8.2"}, {verl, "1.1.1"}]};
config(with_git_deps) ->
    {deps, [
        {hex_core, "0.8.2"}, {verl, "1.1.1"}, {rebar, {git, "git://github.com/erlang/rebar3.git"}}
    ]};
config(with_binary_versions) ->
    {deps, [{hex_core, <<"0.8.2">>}, {verl, <<"1.1.1">>}]};
config(with_alias_deps) ->
    {deps, [{hex_core}, {verl}]}.

locks(with_binary_versions) ->
    locks(default);
locks(with_alias_deps) ->
    locks(default);
locks(default) ->
    [
        {<<"hex_core">>,
            {pkg, <<"hex_core">>, <<"0.8.2">>,
                <<"48D52F273A54F3E1564E9D4E89731EB4C47629A8C0FC03B88C4098B75AEA55CC">>,
                <<"D5E92BD919FC9872FCBCCA1CD13FF094F7BF520EAA29C9D14B674D16A2E1A089">>},
            0},
        {<<"verl">>,
            {pkg, <<"verl">>, <<"1.1.1">>,
                <<"98F3EC48B943AA4AE8E29742DE86A7CD752513687911FE07D2E00ECDF3107E45">>,
                <<"0925E51CD92A0A8BE271765B02430B2E2CFF8AC30EF24D123BD0D58511E8FB18">>},
            0}
    ];
locks(with_git_deps) ->
    Deps = [
        {<<"rebar">>,
            {git, "git://github.com/erlang/rebar3.git",
                {ref, "1af9c6c85c22ae6c11fb3c2b79ee04895c19fee3"}},
            0}
    ],
    locks(default) ++ Deps.

get_app_metadata(Name, Vsn) ->
    {application, erlang:list_to_atom(Name), [
        {description, "An OTP application"},
        {vsn, Vsn},
        {registered, []},
        {applications, []},
        {env, []},
        {modules, []},
        {licenses, ["Apache 2.0"]},
        {links, []}
    ]}.


erl_src_file(Name) ->
    io_lib:format(
        "-module('~s').\n"
        "-export([main/0]).\n"
        "main() -> ok.\n",
        [filename:basename(Name, ".erl")]
    ).

mkdir_p(Path) ->
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).

rebar_state(AppsDir, Repo, Deps) ->
    Config = [
        {root_dir, AppsDir},
        {base_dir, filename:join([AppsDir, "_build"])},
        {command_parsed_args, []},
        {resources, []},
        {hex, [{repos, [Repo]}]}
    ],
    Config1 = rebar_config:merge_locks(Config, Deps),
    State = rebar_state:new(Config1),
    rebar_state:dir(State, AppsDir).

repo_config() ->
    ?REPO_CONFIG.
repo_config(Cfg) ->
    maps:merge(?REPO_CONFIG, Cfg).
