-module(rebar3_hex_repo_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
    [
     format_error_unknown_subcommand_test,
     format_error_missing_repo_name_test,
     format_error_missing_url_test,
     format_error_repo_exists_test,
     format_error_repo_not_found_test,
     format_error_invalid_public_key_test,
     format_error_fetch_public_key_failed_status_test,
     format_error_fetch_public_key_failed_reason_test,
     format_error_public_key_mismatch_test,
     format_error_invalid_fingerprint_format_test,
     format_error_fetch_public_key_requires_url_test,
     format_error_cannot_remove_hexpm_test,
     format_error_unknown_test
    ].

%%%%%%%%%%%%%%%%%%
%%% Test Cases %%%
%%%%%%%%%%%%%%%%%%

format_error_unknown_subcommand_test(_Config) ->
    Result = rebar3_hex_repo:format_error({unknown_subcommand, "foo"}),
    ?assertEqual("Unknown subcommand: foo", lists:flatten(Result)).

format_error_missing_repo_name_test(_Config) ->
    Result = rebar3_hex_repo:format_error(missing_repo_name),
    ?assertEqual("Missing repository name", Result).

format_error_missing_url_test(_Config) ->
    Result = rebar3_hex_repo:format_error({missing_url, "myrepo"}),
    ?assertEqual("Missing URL for repository: myrepo", lists:flatten(Result)).

format_error_repo_exists_test(_Config) ->
    Result = rebar3_hex_repo:format_error({repo_exists, "myrepo"}),
    ?assertEqual("Repository already exists: myrepo. Use 'hex repo set' to update.",
                 lists:flatten(Result)).

format_error_repo_not_found_test(_Config) ->
    Result = rebar3_hex_repo:format_error({repo_not_found, "myrepo"}),
    ?assertEqual("Repository not found: myrepo", lists:flatten(Result)).

format_error_invalid_public_key_test(_Config) ->
    Result = rebar3_hex_repo:format_error({invalid_public_key, "/path/to/key", enoent}),
    ?assertMatch("Failed to read public key from /path/to/key: " ++ _, lists:flatten(Result)).

format_error_fetch_public_key_failed_status_test(_Config) ->
    Result = rebar3_hex_repo:format_error({fetch_public_key_failed, 404}),
    ?assertEqual("Failed to fetch public key: HTTP 404", lists:flatten(Result)).

format_error_fetch_public_key_failed_reason_test(_Config) ->
    Result = rebar3_hex_repo:format_error({fetch_public_key_failed, timeout}),
    ?assertMatch("Failed to fetch public key: " ++ _, lists:flatten(Result)).

format_error_public_key_mismatch_test(_Config) ->
    Result = rebar3_hex_repo:format_error({public_key_mismatch, "SHA256:expected", "SHA256:actual"}),
    Expected = "Public key fingerprint mismatch!\n  Expected: SHA256:expected\n  Got: SHA256:actual",
    ?assertEqual(Expected, lists:flatten(Result)).

format_error_invalid_fingerprint_format_test(_Config) ->
    Result = rebar3_hex_repo:format_error({invalid_fingerprint_format, "badformat"}),
    ?assertMatch("Invalid fingerprint format: badformat" ++ _, lists:flatten(Result)).

format_error_fetch_public_key_requires_url_test(_Config) ->
    Result = rebar3_hex_repo:format_error(fetch_public_key_requires_url),
    ?assertEqual("Cannot fetch public key: repository URL not set. Use --url or add URL first.", Result).

format_error_cannot_remove_hexpm_test(_Config) ->
    Result = rebar3_hex_repo:format_error(cannot_remove_hexpm),
    ?assertEqual("Cannot remove the default hexpm repository", Result).

format_error_unknown_test(_Config) ->
    Result = rebar3_hex_repo:format_error(unknown_error),
    ?assertEqual("An unknown error was encountered. Run with DIAGNOSTIC=1 for more details.", Result).
