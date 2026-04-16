%% @doc `rebar3 hex repo' - Manage Hex repository configuration.
%%
%% Subcommands:
%% - add: Add a new repository
%% - set: Update an existing repository
%% - remove: Remove a repository
%% - show: Display repository configuration
%% - list: List all configured repositories
%%
%% <h2>Add a repository</h2>
%%
%% Adds a new repository configuration.
%%
%% ```
%% $ rebar3 hex repo add NAME URL
%% '''
%%
%% <h2>Update a repository</h2>
%%
%% Updates an existing repository configuration.
%%
%% ```
%% $ rebar3 hex repo set NAME [OPTIONS]
%% '''
%%
%% <h2>Remove a repository</h2>
%%
%% Removes a repository configuration.
%%
%% ```
%% $ rebar3 hex repo remove NAME
%% '''
%%
%% <h2>Show repository configuration</h2>
%%
%% Displays the configuration for a repository.
%%
%% ```
%% $ rebar3 hex repo show NAME
%% '''
%%
%% <h2>List repositories</h2>
%%
%% Lists all configured repositories.
%%
%% ```
%% $ rebar3 hex repo list
%% '''
%%
%% <h2>Command line options</h2>
%%
%% <ul>
%%  <li>`--public-key PATH' - Path to public key PEM file</li>
%%  <li>`--fetch-public-key HASH' - Fetch public key from repository and verify SHA256 fingerprint</li>
%%  <li>`--auth-key KEY' - API key for repository access</li>
%%  <li>`--oauth-exchange' / `--no-oauth-exchange' - Enable/disable OAuth token exchange</li>
%%  <li>`--oauth-exchange-url URL' - Custom OAuth exchange endpoint</li>
%%  <li>`--url URL' - Repository URL (for set command)</li>
%%  <li>`--api-url URL' - API URL (for set command)</li>
%% </ul>
%%
%% <h2>Examples</h2>
%%
%% ```
%% $ rebar3 hex repo add myrepo https://myrepo.example.com
%% $ rebar3 hex repo add myrepo https://myrepo.example.com --auth-key SECRET
%% $ rebar3 hex repo set myrepo --auth-key NEWKEY
%% $ rebar3 hex repo show myrepo
%% $ rebar3 hex repo remove myrepo
%% $ rebar3 hex repo list
%% '''

-module(rebar3_hex_repo).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").

-define(PROVIDER, repo).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
        [{name, ?PROVIDER},
         {module, ?MODULE},
         {namespace, hex},
         {bare, true},
         {deps, ?DEPS},
         {example, "rebar3 hex repo list"},
         {short_desc, "Hex repository configuration commands"},
         {desc, "Manage Hex repository configuration.\n"
                "\n"
                "Subcommands:\n"
                "  add NAME URL    Add a new repository\n"
                "  set NAME        Update an existing repository\n"
                "  remove NAME     Remove a repository\n"
                "  show NAME       Display repository configuration\n"
                "  list            List all configured repositories\n"
                "\n"
                "Options for add/set:\n"
                "  --public-key PATH      Path to public key PEM file\n"
                "  --fetch-public-key HASH  Fetch and verify public key (SHA256:...)\n"
                "  --auth-key KEY         API key for repository access\n"
                "  --oauth-exchange       Enable OAuth token exchange (default for hexpm)\n"
                "  --no-oauth-exchange    Disable OAuth token exchange\n"
                "  --oauth-exchange-url   Custom OAuth exchange endpoint\n"
                "\n"
                "Examples:\n"
                "  rebar3 hex repo add myrepo https://myrepo.example.com\n"
                "  rebar3 hex repo add myrepo https://myrepo.example.com --auth-key SECRET\n"
                "  rebar3 hex repo set myrepo --auth-key NEWKEY\n"
                "  rebar3 hex repo show myrepo\n"
                "  rebar3 hex repo remove myrepo\n"
                "  rebar3 hex repo list\n"},
         {opts, [
            {public_key, undefined, "public-key", string,
             "Path to public key PEM file"},
            {fetch_public_key, undefined, "fetch-public-key", string,
             "Fetch public key from repository and verify SHA256 fingerprint"},
            {auth_key, undefined, "auth-key", string,
             "API key for repository access"},
            {oauth_exchange, undefined, "oauth-exchange", boolean,
             "Enable OAuth token exchange"},
            {oauth_exchange_url, undefined, "oauth-exchange-url", string,
             "Custom OAuth exchange endpoint"},
            {url, undefined, "url", string,
             "Repository URL (for set command)"},
            {api_url, undefined, "api-url", string,
             "API URL (for set command)"}
         ]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case rebar_state:command_args(State) of
        ["add", Name, Url | _] ->
            do_add(Name, Url, Args, State);
        ["add", Name | _] ->
            %% URL might be in --url option
            case proplists:get_value(url, Args) of
                undefined ->
                    ?RAISE({missing_url, Name});
                Url ->
                    do_add(Name, Url, Args, State)
            end;
        ["add" | _] ->
            ?RAISE(missing_repo_name);
        ["set", Name | _] ->
            do_set(Name, Args, State);
        ["set" | _] ->
            ?RAISE(missing_repo_name);
        ["remove", Name | _] ->
            do_remove(Name, State);
        ["remove" | _] ->
            ?RAISE(missing_repo_name);
        ["show", Name | _] ->
            do_show(Name, State);
        ["show" | _] ->
            ?RAISE(missing_repo_name);
        ["list" | _] ->
            do_list(State);
        [] ->
            rebar3_hex_io:say("Usage: rebar3 hex repo <subcommand>"),
            rebar3_hex_io:say(""),
            rebar3_hex_io:say("Subcommands:"),
            rebar3_hex_io:say("  add NAME URL    Add a new repository"),
            rebar3_hex_io:say("  set NAME        Update an existing repository"),
            rebar3_hex_io:say("  remove NAME     Remove a repository"),
            rebar3_hex_io:say("  show NAME       Display repository configuration"),
            rebar3_hex_io:say("  list            List all configured repositories"),
            {ok, State};
        [Unknown | _] ->
            ?RAISE({unknown_subcommand, Unknown})
    end.

%% @private
-spec format_error(any()) -> iolist().
format_error({unknown_subcommand, Cmd}) ->
    io_lib:format("Unknown subcommand: ~ts", [Cmd]);
format_error(missing_repo_name) ->
    "Missing repository name";
format_error({missing_url, Name}) ->
    io_lib:format("Missing URL for repository: ~ts", [Name]);
format_error({repo_exists, Name}) ->
    io_lib:format("Repository already exists: ~ts. Use 'hex repo set' to update.", [Name]);
format_error({repo_not_found, Name}) ->
    io_lib:format("Repository not found: ~ts", [Name]);
format_error({invalid_public_key, Path, Reason}) ->
    io_lib:format("Failed to read public key from ~ts: ~p", [Path, Reason]);
format_error({fetch_public_key_failed, Status}) when is_integer(Status) ->
    io_lib:format("Failed to fetch public key: HTTP ~p", [Status]);
format_error({fetch_public_key_failed, Reason}) ->
    io_lib:format("Failed to fetch public key: ~p", [Reason]);
format_error({public_key_mismatch, Expected, Actual}) ->
    io_lib:format("Public key fingerprint mismatch!~n"
                  "  Expected: ~ts~n"
                  "  Got: ~ts", [Expected, Actual]);
format_error({invalid_fingerprint_format, Fingerprint}) ->
    io_lib:format("Invalid fingerprint format: ~ts~n"
                  "Expected format: SHA256:<base64>", [Fingerprint]);
format_error(fetch_public_key_requires_url) ->
    "Cannot fetch public key: repository URL not set. Use --url or add URL first.";
format_error(cannot_remove_hexpm) ->
    "Cannot remove the default hexpm repository";
format_error(Reason) ->
    rebar3_hex_error:format_error(Reason).

%% ===================================================================
%% Add subcommand
%% ===================================================================

-spec do_add(string(), string(), list(), rebar_state:t()) -> {ok, rebar_state:t()}.
do_add(Name, Url, Args, State) ->
    RepoName = list_to_binary(Name),
    AuthConfig = rebar_hex_repos:auth_config(State),

    %% Check if repo already exists
    case maps:is_key(RepoName, AuthConfig) of
        true ->
            ?RAISE({repo_exists, Name});
        false ->
            ok
    end,

    %% Build repo config
    RepoConfig0 = #{
        repo_url => list_to_binary(Url)
    },

    %% Add optional fields
    RepoConfig1 = maybe_add_api_url(RepoConfig0, Url, Args),
    RepoConfig2 = maybe_add_public_key(RepoConfig1, Args),
    RepoConfig3 = maybe_fetch_public_key(RepoConfig2, Url, Args),
    RepoConfig4 = maybe_add_auth_key(RepoConfig3, Args),
    RepoConfig5 = maybe_add_oauth_exchange(RepoConfig4, Args),
    RepoConfig6 = maybe_add_oauth_exchange_url(RepoConfig5, Args),

    %% Save to hex.config
    rebar_hex_repos:update_repo_auth_config(RepoConfig6, RepoName, State),

    rebar3_hex_io:say("Added repository ~ts", [Name]),
    {ok, State}.

%% ===================================================================
%% Set subcommand
%% ===================================================================

-spec do_set(string(), list(), rebar_state:t()) -> {ok, rebar_state:t()}.
do_set(Name, Args, State) ->
    RepoName = list_to_binary(Name),
    AuthConfig = rebar_hex_repos:auth_config(State),

    %% Get existing config or empty map
    ExistingConfig = maps:get(RepoName, AuthConfig, #{}),

    %% Update fields
    RepoConfig0 = maybe_update_url(ExistingConfig, Args),
    RepoConfig1 = maybe_update_api_url(RepoConfig0, Args),
    RepoConfig2 = maybe_add_public_key(RepoConfig1, Args),
    RepoConfig3 = maybe_fetch_public_key_for_set(RepoConfig2, Args),
    RepoConfig4 = maybe_add_auth_key(RepoConfig3, Args),
    RepoConfig5 = maybe_add_oauth_exchange(RepoConfig4, Args),
    RepoConfig6 = maybe_add_oauth_exchange_url(RepoConfig5, Args),

    %% Save to hex.config
    rebar_hex_repos:update_repo_auth_config(RepoConfig6, RepoName, State),

    rebar3_hex_io:say("Updated repository ~ts", [Name]),
    {ok, State}.

%% ===================================================================
%% Remove subcommand
%% ===================================================================

-spec do_remove(string(), rebar_state:t()) -> {ok, rebar_state:t()}.
do_remove(Name, State) ->
    RepoName = list_to_binary(Name),

    %% Don't allow removing hexpm
    case RepoName of
        <<"hexpm">> ->
            ?RAISE(cannot_remove_hexpm);
        _ ->
            ok
    end,

    AuthConfig = rebar_hex_repos:auth_config(State),

    case maps:is_key(RepoName, AuthConfig) of
        true ->
            rebar_hex_repos:remove_from_auth_config(RepoName, State),
            rebar3_hex_io:say("Removed repository ~ts", [Name]),
            {ok, State};
        false ->
            ?RAISE({repo_not_found, Name})
    end.

%% ===================================================================
%% Show subcommand
%% ===================================================================

-spec do_show(string(), rebar_state:t()) -> {ok, rebar_state:t()}.
do_show(Name, State) ->
    RepoName = list_to_binary(Name),
    AuthConfig = rebar_hex_repos:auth_config(State),

    case maps:find(RepoName, AuthConfig) of
        {ok, RepoConfig} ->
            rebar3_hex_io:say("Repository: ~ts", [Name]),
            rebar3_hex_io:say(""),
            print_repo_config(RepoConfig),
            {ok, State};
        error ->
            %% Check if it's a built-in repo from rebar.config
            try
                {ok, Config} = rebar_hex_repos:get_repo_config(RepoName, State),
                rebar3_hex_io:say("Repository: ~ts (built-in)", [Name]),
                rebar3_hex_io:say(""),
                print_repo_config(Config),
                {ok, State}
            catch
                _:_ ->
                    ?RAISE({repo_not_found, Name})
            end
    end.

%% @private
-spec print_repo_config(map()) -> ok.
print_repo_config(Config) ->
    %% Print URL
    case maps:find(repo_url, Config) of
        {ok, RepoUrl} -> rebar3_hex_io:say("  URL: ~ts", [RepoUrl]);
        error -> ok
    end,

    %% Print API URL
    case maps:find(api_url, Config) of
        {ok, ApiUrl} -> rebar3_hex_io:say("  API URL: ~ts", [ApiUrl]);
        error -> ok
    end,

    %% Print public key (truncated)
    case maps:find(repo_public_key, Config) of
        {ok, PublicKey} when is_binary(PublicKey) ->
            KeyPreview = truncate_key(PublicKey),
            rebar3_hex_io:say("  Public Key: ~ts", [KeyPreview]);
        _ ->
            ok
    end,

    %% Print auth key (masked)
    case maps:find(auth_key, Config) of
        {ok, AuthKey} when is_binary(AuthKey) ->
            Masked = mask_key(AuthKey),
            rebar3_hex_io:say("  Auth Key: ~ts", [Masked]);
        _ ->
            ok
    end,

    %% Print OAuth exchange setting
    case maps:find(oauth_exchange, Config) of
        {ok, OAuthExchange} ->
            rebar3_hex_io:say("  OAuth Exchange: ~p", [OAuthExchange]);
        error ->
            ok
    end,

    %% Print OAuth exchange URL
    case maps:find(oauth_exchange_url, Config) of
        {ok, OAuthUrl} ->
            rebar3_hex_io:say("  OAuth Exchange URL: ~ts", [OAuthUrl]);
        error ->
            ok
    end,

    ok.

%% @private
-spec truncate_key(binary()) -> binary().
truncate_key(Key) ->
    case byte_size(Key) > 50 of
        true ->
            <<Prefix:40/binary, _/binary>> = Key,
            <<Prefix/binary, "...">>;
        false ->
            Key
    end.

%% @private
-spec mask_key(binary()) -> binary().
mask_key(Key) ->
    case byte_size(Key) of
        Len when Len > 8 ->
            <<Prefix:4/binary, _/binary>> = Key,
            <<Prefix/binary, "****">>;
        _ ->
            <<"****">>
    end.

%% ===================================================================
%% List subcommand
%% ===================================================================

-spec do_list(rebar_state:t()) -> {ok, rebar_state:t()}.
do_list(State) ->
    AuthConfig = rebar_hex_repos:auth_config(State),

    %% Get repos from auth config (excluding $oauth)
    AuthRepos = [Name || Name <- maps:keys(AuthConfig), Name =/= <<"$oauth">>],

    %% Get repos from rebar.config
    Resources = rebar_state:resources(State),
    ConfigRepos = case rebar_resource_v2:find_resource_state(pkg, Resources) of
        #{repos := Repos} ->
            [maps:get(name, R) || R <- Repos];
        _ ->
            []
    end,

    %% Combine and deduplicate
    AllRepos = lists:usort(AuthRepos ++ ConfigRepos),

    case AllRepos of
        [] ->
            rebar3_hex_io:say("No repositories configured.");
        _ ->
            rebar3_hex_io:say("Repositories:"),
            lists:foreach(fun(RepoName) ->
                Source = case {lists:member(RepoName, AuthRepos), lists:member(RepoName, ConfigRepos)} of
                    {true, true} -> " (hex.config + rebar.config)";
                    {true, false} -> " (hex.config)";
                    {false, true} -> " (rebar.config)";
                    {false, false} -> ""
                end,
                rebar3_hex_io:say("  ~ts~ts", [RepoName, Source])
            end, AllRepos)
    end,
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private
-spec maybe_add_api_url(map(), string(), list()) -> map().
maybe_add_api_url(Config, Url, Args) ->
    case proplists:get_value(api_url, Args) of
        undefined ->
            %% Derive API URL from repo URL
            ApiUrl = derive_api_url(Url),
            Config#{api_url => list_to_binary(ApiUrl)};
        ApiUrl ->
            Config#{api_url => list_to_binary(ApiUrl)}
    end.

%% @private
-spec derive_api_url(string()) -> string().
derive_api_url(Url) ->
    %% Simple heuristic: append /api if not already present
    case lists:suffix("/api", Url) of
        true -> Url;
        false ->
            case lists:suffix("/", Url) of
                true -> Url ++ "api";
                false -> Url ++ "/api"
            end
    end.

%% @private
-spec maybe_update_url(map(), list()) -> map().
maybe_update_url(Config, Args) ->
    case proplists:get_value(url, Args) of
        undefined -> Config;
        Url -> Config#{repo_url => list_to_binary(Url)}
    end.

%% @private
-spec maybe_update_api_url(map(), list()) -> map().
maybe_update_api_url(Config, Args) ->
    case proplists:get_value(api_url, Args) of
        undefined -> Config;
        ApiUrl -> Config#{api_url => list_to_binary(ApiUrl)}
    end.

%% @private
-spec maybe_add_public_key(map(), list()) -> map().
maybe_add_public_key(Config, Args) ->
    case proplists:get_value(public_key, Args) of
        undefined ->
            Config;
        Path ->
            case file:read_file(Path) of
                {ok, PemData} ->
                    Config#{repo_public_key => PemData};
                {error, Reason} ->
                    ?RAISE({invalid_public_key, Path, Reason})
            end
    end.

%% @private
-spec maybe_fetch_public_key(map(), string(), list()) -> map().
maybe_fetch_public_key(Config, Url, Args) ->
    case proplists:get_value(fetch_public_key, Args) of
        undefined ->
            Config;
        ExpectedFingerprint ->
            fetch_and_verify_public_key(Config, Url, ExpectedFingerprint)
    end.

%% @private
-spec maybe_fetch_public_key_for_set(map(), list()) -> map().
maybe_fetch_public_key_for_set(Config, Args) ->
    case proplists:get_value(fetch_public_key, Args) of
        undefined ->
            Config;
        ExpectedFingerprint ->
            case maps:find(repo_url, Config) of
                {ok, Url} ->
                    fetch_and_verify_public_key(Config, binary_to_list(Url),
                                                ExpectedFingerprint);
                error ->
                    ?RAISE(fetch_public_key_requires_url)
            end
    end.

%% @private
-spec fetch_and_verify_public_key(map(), string(), string()) -> map().
fetch_and_verify_public_key(Config, Url, ExpectedFingerprint) ->
    %% Build config for HTTP request by merging with defaults
    RepoUrl = list_to_binary(Url),
    FetchConfig = maps:merge(hex_core:default_config(), #{
        repo_url => RepoUrl,
        http_adapter => {hex_http_httpc, #{profile => default}}
    }),

    case hex_repo:get_public_key(FetchConfig) of
        {ok, {200, _Headers, PublicKey}} ->
            verify_and_store_key(Config, PublicKey, ExpectedFingerprint);
        {ok, {Status, _Headers, _Body}} ->
            ?RAISE({fetch_public_key_failed, Status});
        {error, Reason} ->
            ?RAISE({fetch_public_key_failed, Reason})
    end.

%% @private
-spec verify_and_store_key(map(), binary(), string()) -> map().
verify_and_store_key(Config, PublicKey, ExpectedFingerprint) ->
    case hex_repo:fingerprint_equal(PublicKey, ExpectedFingerprint) of
        true ->
            Config#{repo_public_key => PublicKey};
        false ->
            ActualFingerprint = hex_repo:fingerprint(PublicKey),
            ?RAISE({public_key_mismatch, ExpectedFingerprint, ActualFingerprint})
    end.

%% @private
-spec maybe_add_auth_key(map(), list()) -> map().
maybe_add_auth_key(Config, Args) ->
    case proplists:get_value(auth_key, Args) of
        undefined -> Config;
        AuthKey -> Config#{auth_key => list_to_binary(AuthKey)}
    end.

%% @private
-spec maybe_add_oauth_exchange(map(), list()) -> map().
maybe_add_oauth_exchange(Config, Args) ->
    case proplists:get_value(oauth_exchange, Args) of
        undefined -> Config;
        Value -> Config#{oauth_exchange => Value}
    end.

%% @private
-spec maybe_add_oauth_exchange_url(map(), list()) -> map().
maybe_add_oauth_exchange_url(Config, Args) ->
    case proplists:get_value(oauth_exchange_url, Args) of
        undefined -> Config;
        Url -> Config#{oauth_exchange_url => list_to_binary(Url)}
    end.
