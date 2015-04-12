-module(rebar3_hex_config).

-export([path/0
        ,read/0
        ,api_url/0
        ,cdn_url/0
        ,auth/0
        ,update/1]).

-define(DEFAULT_HEX_CONFIG, "hex.config").
-define(DEFAULT_API_URL, "https://hex.pm").
-define(DEFAULT_CDN_URL, "https://s3.amazonaws.com/s3.hex.pm").

path() ->
    filename:join(rebar3_hex_utils:hex_home(), ?DEFAULT_HEX_CONFIG).

read() ->
    case file:consult(path()) of
        {ok, Config} ->
            Config;
        _ ->
            []
    end.

api_url() ->
    proplists:get_value(api_url, read(), ?DEFAULT_API_URL).

cdn_url() ->
    proplists:get_value(cdn_url, read(), ?DEFAULT_CDN_URL).

auth() ->
    case lists:keyfind(key, 1, read()) of
        {key, Key} ->
            {ok, Key};
        _ ->
            error
    end.

update(NewConfig) ->
    Config = read(),
    Config1 = lists:ukeymerge(1
                             ,lists:keysort(1, NewConfig)
                             ,lists:keysort(1, Config)),
    file:write_file(path(), encode_config(Config1)).

%% Internal functions

encode_config(Config) ->
    iolist_to_binary([[io_lib:print(Term) | ".\n"] || Term <- Config]).
