-module(rebar3_hex_tar).

-export([create/4]).

-include("rebar3_hex.hrl").

create(Name, Version, Meta, Files) ->
    ContentsPath = io_lib:format("~s-~s-contents.tar.gz", [Name, Version]),
    Path = io_lib:format("~s-~s.tar", [Name, Version]),
    ok = erl_tar:create(ContentsPath, Files, [compressed]),

    {ok, Contents} = file:read_file(ContentsPath),
    MetaString = encode_term(Meta),

    Blob = <<(?VERSION)/binary, MetaString/binary, Contents/binary>>,
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Blob),
    Checksum = string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X]))),

    MetaFiles = [
                {"VERSION", ?VERSION},
                {"CHECKSUM", list_to_binary(Checksum)},
                {"metadata.config", MetaString},
                {"contents.tar.gz", Contents}
                ],

    ok = erl_tar:create(Path, MetaFiles),
    Tar = file:read_file(Path),
    file:delete(ContentsPath),
    file:delete(Path),
    Tar.

encode_term(Meta) ->
    Data = lists:map(fun(MetaPair) ->
        String = io_lib_pretty:print(rebar3_hex_utils:binarify(MetaPair), [{encoding, utf8}]),
        unicode:characters_to_binary([String, ".\n"])
    	end, Meta),
    iolist_to_binary(Data).
