-module(rebar3_hex_pkg).

-export([publish/6
        ,format_error/1]).

-include("rebar3_hex.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

publish(Name, Version, Description, _Deps, Files, Hex) ->
    {ok, Cwd} = file:get_cwd(),
    Files = expand_paths(proplists:get_value(files, Hex, ?DEFAULT_FILES), Cwd),
    Licenses = proplists:get_value(licenses, Hex, []),
    Links = proplists:get_value(links, Hex, []),

    Meta = [{name, Name}
           ,{version, Version}
           ,{app, Name}
           ,{precompiled, true}
           ,{parameters, [{<<"erts">>, <<"6.2">>}]}
           ,{description, Description}
           ,{files, Files}
           ,{licenses, Licenses}
           ,{links, Links}
           ,{requirements, []}],

    MetaString = jsx:encode([{<<"meta">>, rebar3_hex_utils:binarify(Meta)}]),
    {ok, Auth} = binary_to_list(rebar3_hex_config:auth()),

    case rebar3_hex_http:put("packages/minasan", Auth, MetaString) of
        {ok, {{_, 200, _}, _, _RespBody}} ->
            ec_talk:say("~s~n~s~n~p~n", [Name, Version, Description]),
            %% Cont = ec_talk:ask_default("Continue?", boolean, "Y"),

            {ok, Tar} = rebar3_hex_tar:create(Name, Version, Meta, Files),

            Body = fun(Size) when Size < byte_size(Tar) ->
                           NewSize = min(Size + ?CHUNK, byte_size(Tar)),
                           Chunk = NewSize - Size,
                           {ok, [binary:part(Tar, Size, Chunk)], NewSize};
                      (_Size) ->
                           eof
                   end,
            rebar3_hex_http:post("packages/minasan/releases", Auth, Body);
        {ok, {{_, _Status, _}, _RespHeaders, RespBodyJson}} ->
            RespBody = jsx:decode(list_to_binary(RespBodyJson)),
            {error, RespBody}
    end.

-spec format_error(any()) -> iolist().
format_error(undefined_server_error) ->
    "Unknown server error";
format_error(Error) ->
    Message = proplists:get_value(<<"message">>, Error, ""),
    Errors = proplists:get_value(<<"errors">>, Error, ""),
    ErrorString = errors_to_string(Errors),

    io_lib:format("Hex Error: ~s~n\t~s", [Message, ErrorString]).

%% Internal functions

errors_to_string(Value) when is_binary(Value) ->
    Value;
errors_to_string({Key, Value}) ->
    io_lib:format("~s: ~s", [Key, errors_to_string(Value)]);
errors_to_string(Errors) when is_list(Errors) ->
    lists:flatten([io_lib:format("~s", [errors_to_string(Values)]) || Values <- Errors]).

expand_paths(Paths, Dir) ->
    AbsDir = filename:absname(Dir),
    Files = lists:flatmap(fun(X) ->
                                  lists:flatmap(fun(Y) ->
                                                        dir_files(Y)
                                                end, filelib:wildcard(X))
                          end, [filename:join(Dir, P) || P <- Paths]),
    [F1 -- (AbsDir++"/") || F1 <- lists:filter(fun filelib:is_regular/1, [filename:absname(F) || F <- Files])].

dir_files(Path) ->
    case filelib:is_dir(Path) of
        true ->
             filelib:wildcard(filename:join(Path, "**"));
        false ->
            [Path]
    end.
