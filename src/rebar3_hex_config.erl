%% @private
-module(rebar3_hex_config).

-export([ api_key_name/1
        , api_key_name/2
        , all_repos/1
        , repos_key_name/0
        , org_key_name/2
        , parent_repos/1
        , default_repo/1
        , repo/1
        , repo/2
        , set_http_adapter/1
        , update_auth_config/2
        ]).

-include("rebar3_hex.hrl").

-type repo_error() :: {not_valid_repo, string()} | no_repo_in_state | {required, repo}.
-export_type([repo_error/0]).

-spec api_key_name(binary()) -> binary().
api_key_name(Key) ->
    Prefix = key_name_prefix(Key),
    key_name(Prefix, <<"-api">>).

-spec api_key_name(binary(), binary()) -> binary().
api_key_name(Key, Suffix) ->
     Prefix = key_name_prefix(Key),
     key_name(Prefix, <<"-api-">>, Suffix).

-spec repos_key_name() -> binary().
repos_key_name() ->
     key_name(hostname(), <<"-repositories">>).

-spec org_key_name(binary(), binary()) -> binary().
org_key_name(Key, Org) ->
     Prefix = key_name_prefix(Key),
     key_name(Prefix, <<"-repository-">>, Org).

-spec hostname() -> binary().
hostname() ->
    {ok, Name} = inet:gethostname(),
    list_to_binary(Name).

key_name(Prefix, Suffix) ->
    <<Prefix/binary, Suffix/binary>>.

key_name(Prefix, Interfix, Suffix) ->
    <<Prefix/binary, Interfix/binary, Suffix/binary>>.

key_name_prefix(undefined) -> hostname();
key_name_prefix(Key) -> Key.

update_auth_config(Config, State) ->
    rebar_hex_repos:update_auth_config(Config, State).

all_repos(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    Repos.

-spec repo(rebar_state:t()) -> {ok, map()} | {error, repo_error()}.
repo(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Repos = all_repos(State),
    case proplists:get_value(repo, Args, undefined) of
        undefined ->
            Res = [R || R <- Repos, maps:get(name, R) =/= ?DEFAULT_HEX_REPO],
            case Res of
                [] ->
                    repo(State, ?DEFAULT_HEX_REPO);
                [_Repo|_Rest] ->
                    {error, {required, repo}}
            end;
        RepoName ->
            repo(State, RepoName)
    end.

repo(State, RepoName) ->
    BinName = rebar_utils:to_binary(RepoName),
    try rebar_hex_repos:get_repo_config(BinName, State) of
        {ok, Repo} ->
            {ok, set_http_adapter(Repo)}
    catch
        throw:{error, {rebar_hex_repos, {repo_not_found, _}}} ->
            {error, {not_valid_repo, RepoName}}
    end.

set_http_adapter(Repo) ->
    Repo#{http_adapter => {rebar3_hex_httpc_adapter, #{profile => rebar}}}.

parent_repos(State) ->
    Fun = fun(#{name := Name} = Repo, Acc) ->
                  [Parent|_] = rebar3_hex_io:str_split(Name, <<":">>),
                  case maps:is_key(Parent, Acc) of
                    true ->
                        Acc;
                    false ->
                        maps:put(name, set_http_adapter(Repo), Acc)
                  end
          end,
    Map = lists:foldl(Fun, #{}, all_repos(State)),
    maps:values(Map).

default_repo(State) ->
    rebar_hex_repos:get_repo_config(?DEFAULT_HEX_REPO, all_repos(State)).
