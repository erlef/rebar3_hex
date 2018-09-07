-module(rebar3_hex_config).

-export([init/1,
         do/1,
         format_error/1]).

-export([path/1
        ,read/1
        ,api_url/1
        ,cdn_url/1
        ,username/1
        ,auth/1
        ,http_proxy/1
        ,https_proxy/1
        ,update/2
        ,write/2]).

-define(PROVIDER, config).
-define(DEPS, []).

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
            maybe_update(username, Value, State),
            ec_talk:say("username: ~s", [username(State)]);
        ["key" | Value] ->
            maybe_update(key, Value, State),
            {ok, Key} = auth(State),
            ec_talk:say("key: ~s", [Key]);
        ["api_url" | Value] ->
            maybe_update(api_url, Value, State),
            ec_talk:say("api_url: ~s", [api_url(State)]);
        ["cdn_url" | Value] ->
            maybe_update(cdn_url, Value, State),
            ec_talk:say("cdn_url: ~s", [cdn_url(State)]);
        ["http_proxy" | Value] ->
            maybe_update(http_proxy, Value, State),
            ec_talk:say("http_proxy: ~s", [http_proxy(State)]);
        ["https_proxy" | Value] ->
            maybe_update(https_proxy, Value, State),
            ec_talk:say("https_proxy: ~s", [https_proxy(State)]);
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

path(State) ->
    filename:join(rebar_dir:global_config_dir(State)).

read(State) ->
    case file:consult(path(State)) of
        {ok, [Config]} ->
            Config;
        _ ->
            #{}
    end.

api_url(State) ->
    proplists:get_value(api_url, read(State), ?DEFAULT_API_URL).

cdn_url(State) ->
    case os:getenv("HEX_CDN") of
        false ->
            proplists:get_value(cdn_url, read(State), ?DEFAULT_CDN_URL);
        CDN ->
            CDN
    end.

username(State) ->
    proplists:get_value(username, read(State), undefined).

http_proxy(State) ->
    proplists:get_value(http_proxy, read(State), "").

https_proxy(State) ->
    proplists:get_value(https_proxy, read(State), "").

auth(State) ->
    case lists:keyfind(key, 1, read(State)) of
        {key, Key} ->
            {ok, Key};
        _ ->
            throw({error, {?MODULE, no_user}})
    end.

update(NewConfig, State) ->
    Config = read(State),
    Config1 = lists:ukeymerge(1
                             ,lists:keysort(1, NewConfig)
                             ,lists:keysort(1, Config)),
    write(Config1, State).

write(Config, State) ->
    filelib:ensure_dir(path(State)),
    ok = file:write_file(path(State), encode_config([Config])).

%% Internal functions

maybe_update(_, [], _) ->
    ok;
maybe_update(Key, Value, State) ->
    update([{Key, ec_cnv:to_binary(Value)}], State).

encode_config(Config) ->
    iolist_to_binary([[io_lib:print(Term) | ".\n"] || Term <- Config]).
