-module(rebar3_hex_org).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, org).
-define(DEPS, []).

-define(ENDPOINT, "organizations").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {namespace, hex},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 hex org <command>"},
                                 {short_desc, "Hex organization tasks"},
                                 {desc, ""},
                                 {opts, [rebar3_hex:repo_opt(), {key,$k, "key", string, "key key key"}]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["auth" | _] ->
            {Args, _} = rebar_state:command_parsed_args(State),
            KeyName = proplists:get_value(key, Args),
            {ok, Repo} = rebar3_hex_config:repo(State),
            auth(Repo, State, KeyName);
        ["deauth" | _] ->
            {ok, Repo} = rebar3_hex_config:repo(State),
            deauth(Repo, State);
        ["list" | _] ->
            {ok, Repo} = rebar3_hex_config:repo(State),
            %{ok, Read} = rebar3_hex_config:hex_config_write(Repo),
            list(State, Repo);
        _ ->
            throw(?PRV_ERROR(bad_command))
    end.

%% TODO:
%% mix hex.organization auth ORGANIZATION -- key
% mix hex.organization deauth ORGANIZATION --key
% mix hex.organization list --key ?
% mix hex.organization key ORGANIZATION generate
% mix hex.organization key ORGANIZATION revoke KEY_NAME
% mix hex.organization key ORGANIZATION revoke --all
% mix hex.organization key ORGANIZATION list

list(State, Repo) ->
    {success, Res} = rebar3_hex_client:member_of(Repo#{api_key => maps:get(read_key, Repo)}),
    print_results(Res),
    {ok, State}.

print_results(Res) ->
    Header = ["Name", "Created"],
    Rows = lists:map(fun(#{<<"name">> := Name, <<"inserted_at">> := Created}) ->
                                [binary_to_list(Name), binary_to_list(Created)]
                     end, Res),
    ok = rebar3_hex_results:print_table([Header] ++ Rows),
    ok.

deauth(#{name := Name} = _Repo, State) ->
    ok = rebar3_hex_config:update_auth_config(#{Name => #{}}, State),
    {ok, State}.

auth(#{name := Name} = Repo, State, undefined) ->
    {ok, Config} = rebar3_hex_config:hex_config_write(Repo),
    #{api_organization := Org} = Config,
    Perms = [#{domain => <<"repository">>, resource => Org}],
    case rebar3_hex_client:key_add(Config, <<"hmmmm">>, Perms) of
        {created, #{<<"secret">> := BinKey}} ->
            ok = rebar3_hex_config:update_auth_config(#{Name => #{auth_key => BinKey}}, State),
            {ok, State};
        Error ->
            ?PRV_ERROR({generate, Error})
    end;
auth(#{name := Name} = Repo, State, Key) ->
	case test_key(State, Repo, Key) of
        {ok, State} ->
            BinKey = list_to_binary(Key),
            ok = rebar3_hex_config:update_auth_config(#{Name => #{auth_key => BinKey}}, State),
            {ok, State};
        {error, _Reason} = Err ->
            Err
    end.

test_key(State, #{api_organization := Org} = Repo, Key) ->
    Config = Repo#{api_key => list_to_binary(Key)},

    case rebar3_hex_client:test_key(Config, #{domain => <<"repository">>, resource => Org}) of
        {ok, _Res} ->
            {ok, State};
        _Other ->
          Reason = "Failed to authenticate against organization repository with given key",
          {error, Reason}
    end.


-spec format_error(any()) -> iolist().
format_error(bad_local_password) ->
    "Failure to decrypt write key: bad local password";
format_error({registration_failure, Reason}) ->
    io_lib:format("Registration of user failed: ~ts", [Reason]);
format_error({generate_key, Reason}) ->
    io_lib:format("Failure generating authentication tokens: ~ts", [Reason]);
format_error(no_match_local_password) ->
    "Password confirmation failed. The passwords must match.";
format_error(bad_command) ->
    "Command must be one of register, whoami, auth, deauth or reset_password";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).
