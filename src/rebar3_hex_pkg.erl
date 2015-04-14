-module(rebar3_hex_pkg).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([publish/7]).

-include("rebar3_hex.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, publish).
-define(DEPS, [{default, app_discovery}]).

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
                                {example, "rebar3 hex publish"},
                                {short_desc, "."},
                                {desc, ""},
                                {opts, []}
                                ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    HexOptions = rebar_state:get(State, hex, []),
    [App] = rebar_state:project_apps(State),
    AppDir = rebar_app_info:dir(App),
    Name = rebar_app_info:name(App),
    Version = rebar_app_info:original_vsn(App),
    Deps = rebar_state:get(State, {locks, default}, []),
    TopLevel = [{N, V} || {_,{pkg,N,V},0} <- Deps],
    Excluded = [binary_to_list(N) || {N,{T,_,_},0} <- Deps, T =/= pkg],
    AppDetails = rebar_app_info:app_details(App),
    Description = list_to_binary(proplists:get_value(description, AppDetails, "")),
    case publish(AppDir, Name, Version, Description, TopLevel, Excluded, HexOptions) of
        ok ->
            {ok, State};
        Error ->
            Error
    end.

-spec format_error(any()) -> iolist().
format_error(undefined_server_error) ->
    "Unknown server error";
format_error(Error) ->
    Message = proplists:get_value(<<"message">>, Error, ""),
    Errors = proplists:get_value(<<"errors">>, Error, ""),
    ErrorString = errors_to_string(Errors),

    io_lib:format("Hex Error: ~s~n\t~s", [Message, ErrorString]).

%% ===================================================================
%% Public API
%% ===================================================================

publish(AppDir, Name, Version, Description, Deps, Excluded, HexOptions) ->
    FilePaths = proplists:get_value(files, HexOptions, ?DEFAULT_FILES),
    Files = rebar3_hex_utils:expand_paths(FilePaths, AppDir),
    Contributors = proplists:get_value(contributors, HexOptions, []),
    Licenses = proplists:get_value(licenses, HexOptions, []),
    Links = proplists:get_value(links, HexOptions, []),

    Optional = [{app, Name}
               ,{requirements, Deps}
               ,{contributors, Contributors}
               ,{precompiled, false}
               ,{parameters, []}
               ,{description, Description}
               ,{files, Files}
               ,{licenses, Licenses}
               ,{links, Links}
               ,{requirements, []}],
    OptionalFiltered = [{Key, Value} || {Key, Value} <- Optional, Value =/= []],

    Meta = [{name, Name}, {version, Version} | OptionalFiltered],

    MetaString = [{<<"meta">>, rebar3_hex_utils:binarify(Meta)}],
    {ok, Auth} = rebar3_hex_config:auth(),

    case create_package(Auth, Name, MetaString) of
        ok ->
            ec_talk:say("Publishing ~s ~s", [Name, Version]),
            ec_talk:say("  Dependencies:~n    ~s", [format_deps(Deps)]),
            ec_talk:say("  Excluded dependencies (not part of the Hex package):~n    ~s", [string:join(Excluded, "\n    ")]),
            ec_talk:say("  Included files:~n    ~s", [string:join(Files, "\n    ")]),
            case ec_talk:ask_default("Proceed?", boolean, "Y") of
                true ->
                    upload_package(Auth, Name, Version, Meta, Files);
                _ ->
                    ec_talk:say("Goodbye..."),
                    ok
            end;
        {error, Error} ->
            ?PRV_ERROR(Error)
    end.

%% Internal functions

create_package(Auth, Name, MetaString) ->
    rebar3_hex_http:put(filename:join([?ENDPOINT, Name]), Auth, MetaString).

upload_package(Auth, Name, Version, Meta, Files) ->
    {ok, Tar} = rebar3_hex_tar:create(Name, Version, Meta, Files),

    Body = fun(Size) when Size < byte_size(Tar) ->
                   NewSize = min(Size + ?CHUNK, byte_size(Tar)),
                   Chunk = NewSize - Size,
                   {ok, [binary:part(Tar, Size, Chunk)], NewSize};
              (_Size) ->
                   eof
           end,

    case rebar3_hex_http:post(filename:join([?ENDPOINT, Name, "releases"])
                             ,Auth
                             ,Body
                             ,integer_to_list(byte_size(Tar))) of
        ok ->
            ec_talk:say("Published ~s ~s", [Name, Version]),
            ok;
        {error, Error} ->
            ?PRV_ERROR(Error)
    end.

errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string({Key, Value}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Value)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Values)]) || Values <- Errors]).

format_deps(Deps) ->
    string:join([binary_to_list(<<N/binary, " ", V/binary>>) || {N, V} <- Deps], "\n    ").
