-module(rebar3_hex_config).

-export([init/1,
         do/1,
         format_error/1]).

-export([path/0
        ,read/0
        ,api_url/0
        ,cdn_url/0
        ,username/0
        ,auth/0
        ,http_proxy/0
        ,https_proxy/0
        ,update/1
        ,write/1]).

-define(PROVIDER, config).
-define(DEPS, []).

-define(DEFAULT_HEX_CONFIG, "hex.config").
-define(DEFAULT_API_URL, "https://hex.pm").
-define(DEFAULT_CDN_URL, "https://s3.amazonaws.com/s3.hex.pm").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 hex config <key> [<value>]"},
                                {short_desc, "Read or update hex configuration file"},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["username" | Value] ->
            maybe_update(username, Value),
            ec_talk:say("username: ~s", [username()]);
        ["key" | Value] ->
            maybe_update(key, Value),
            {ok, Key} = auth(),
            ec_talk:say("key: ~s", [Key]);
        ["api_url" | Value] ->
            maybe_update(api_url, Value),
            ec_talk:say("api_url: ~s", [api_url()]);
        ["cdn_url" | Value] ->
            maybe_update(cdn_url, Value),
            ec_talk:say("cdn_url: ~s", [cdn_url()]);
        ["http_proxy" | Value] ->
            maybe_update(http_proxy, Value),
            ec_talk:say("http_proxy: ~s", [http_proxy()]);
        ["https_proxy" | Value] ->
            maybe_update(https_proxy, Value),
            ec_talk:say("https_proxy: ~s", [https_proxy()]);
        [Other | _]->
            rebar_api:error("Config does not contain a key ~s", [Other]);
        [] ->
            rebar_api:info("Usage: rebar3 hex config <key> [<value>]", [])
    end,

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(no_user) ->
    ("No user registered. Either register or, if you have registered, "
     "please auth. See https://hex.pm/docs/rebar3_publish for instructions.");
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

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
    case os:getenv("HEX_CDN") of
        false ->
            proplists:get_value(cdn_url, read(), ?DEFAULT_CDN_URL);
        CDN ->
            CDN
    end.

username() ->
    proplists:get_value(username, read(), "").

http_proxy() ->
    proplists:get_value(http_proxy, read(), "").

https_proxy() ->
    proplists:get_value(https_proxy, read(), "").

auth() ->
    case lists:keyfind(key, 1, read()) of
        {key, Key} ->
            {ok, Key};
        _ ->
            throw({error, {?MODULE, no_user}})
    end.

update(NewConfig) ->
    Config = read(),
    Config1 = lists:ukeymerge(1
                             ,lists:keysort(1, NewConfig)
                             ,lists:keysort(1, Config)),
    write(Config1).

write(Config) ->
    filelib:ensure_dir(path()),
    ok = file:write_file(path(), encode_config(Config)).

%% Internal functions

maybe_update(_, []) ->
    ok;
maybe_update(Key, Value) ->
    update([{Key, ec_cnv:to_binary(Value)}]).

encode_config(Config) ->
    iolist_to_binary([[io_lib:print(Term) | ".\n"] || Term <- Config]).
