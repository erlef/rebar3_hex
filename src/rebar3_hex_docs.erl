-module(rebar3_hex_docs).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar3_hex.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, docs).
-define(DEPS, [{default, edoc}]).

-define(ENDPOINT, "packages").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {namespace, hex},
                                {bare, false},
                                {deps, ?DEPS},
                                {example, "rebar3 hex docs"},
                                {short_desc, "."},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [App] = rebar_state:project_apps(State),
    AppDir = rebar_app_info:dir(App),
    Files = rebar3_hex_utils:expand_paths(["doc"], AppDir),

    Name = binary_to_list(rebar_app_info:name(App)),
    Vsn = rebar_app_info:original_vsn(App),

    Tarball = Name++"-"++Vsn++"-docs.tar.gz",
    ok = erl_tar:create(Tarball, Files),
    {ok, Tar} = file:read_file(Tarball),

    %file:delete(Tarball),

    Body = fun(Size) when Size < byte_size(Tar) ->
                   NewSize = min(Size + ?CHUNK, byte_size(Tar)),
                   Chunk = NewSize - Size,
                   {ok, [binary:part(Tar, Size, Chunk)], NewSize};
              (_Size) ->
                   eof
           end,

    {ok, Auth} = rebar3_hex_config:auth(),
    case rebar3_hex_http:post(filename:join([?ENDPOINT, Name, "releases", Vsn, "docs"])
                             ,Auth
                             ,Body
                             ,integer_to_list(byte_size(Tar))) of
        ok ->
            ec_talk:say("Published docs for ~s ~s", [Name, Vsn]),
            {ok, State};
        {error, Error} ->
            ?PRV_ERROR(Error)
    end.

-spec format_error(any()) -> iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).
