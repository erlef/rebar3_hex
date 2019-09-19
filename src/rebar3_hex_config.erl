-module(rebar3_hex_config).

-export([parent_repos/1,
         hex_config_write/1,
         hex_config_read/1,
         repo/1,
         update_auth_config/2
        ]).

-include("rebar3_hex.hrl").

update_auth_config(Config, State) ->
    rebar_hex_repos:update_auth_config(Config, State).

repo(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    case proplists:get_value(repo, Args, undefined) of
        undefined ->
            DefaultBinName = rebar_utils:to_binary(?DEFAULT_HEX_REPO),
            Res = lists:filter(fun(R) -> maps:get(name, R) =/= DefaultBinName end,
                               Repos),
            case Res of
                [] ->
                    case rebar_hex_repos:get_repo_config(rebar_utils:to_binary(?DEFAULT_HEX_REPO), Repos) of
                        {ok, Repo} ->
                            {ok, Repo};
                        _ ->
                            {error, no_repo_in_state}
                    end;
                [_Repo|_Rest] ->
                    {error, {required, repo}}
            end;
        RepoName ->
            repo(State, RepoName)
    end.

repo(State, RepoName) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    BinName = rebar_utils:to_binary(RepoName),
    MaybeFound1 = get_repo(BinName, Repos),
    MaybeParentRepo = <<"hexpm:">>,
    MaybeFound2 =  get_repo(<<MaybeParentRepo/binary, BinName/binary>>, Repos),
    case {MaybeFound1, MaybeFound2} of
        {{ok, Repo1}, undefined} ->
            {ok, Repo1};
        {undefined, {ok, Repo2}} ->
            {ok, Repo2};
        {undefined, undefined} ->
            {error, {not_valid_repo, RepoName}}
    end.

parent_repos(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    Fun = fun(#{name := Name} = Repo, Acc) ->
                  [Parent|_] = rebar3_hex_io:str_split(Name, <<":">>),
                  case maps:is_key(Parent, Acc) of
                    true ->
                        Acc;
                    false ->
                        maps:put(name, Repo, Acc)
                  end
          end,
    Map = lists:foldl(Fun, #{}, Repos),
    {ok, maps:values(Map)}.

get_repo(BinaryName, Repos) ->
    try rebar_hex_repos:get_repo_config(BinaryName, Repos) of
        Name ->
            Name
    catch
        {error,{rebar_hex_repos,{repo_not_found,BinaryName}}} -> undefined
    end.

hex_config_write(#{write_key := undefined}) ->
    {error, no_write_key};
hex_config_write(#{api_key := <<_ApiKey/binary>>} = HexConfig) ->
    {ok, HexConfig};
hex_config_write(#{write_key := WriteKey, username := Username} = HexConfig) ->
    DecryptedWriteKey = rebar3_hex_user:decrypt_write_key(Username, WriteKey),
    {ok, HexConfig#{api_key => DecryptedWriteKey}}.

hex_config_read(#{read_key := ReadKey} = HexConfig) ->
    {ok, HexConfig#{api_key => ReadKey}};
hex_config_read(_Config) ->
    {error, no_read_key}.
