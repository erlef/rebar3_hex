-module(rebar3_hex_user_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     format_error_not_authenticated_test,
     format_error_whoami_test,
     format_error_registration_failure_test,
     format_error_generate_key_test,
     format_error_passwords_do_not_match_test,
     format_error_reset_account_password_test,
     format_error_bad_command_test,
     format_error_input_required_test,
     format_error_key_revoke_test,
     format_error_key_revoke_all_test,
     format_error_key_list_test,
     format_error_auth_failed_test,
     format_error_revoke_failed_test,
     format_error_unknown_test
    ].

%% format_error tests for auth/deauth related errors

format_error_not_authenticated_test(_Config) ->
    ?assertEqual(<<"Not authenticated as any user currently for this repository">>,
                 list_to_bitstring(rebar3_hex_user:format_error(not_authenticated))).

format_error_whoami_test(_Config) ->
    ?assertEqual(<<"Fetching currently authenticated user failed: eh">>,
                 list_to_bitstring(rebar3_hex_user:format_error({whoami, <<"eh">>}))).

format_error_registration_failure_test(_Config) ->
    ?assertEqual(<<"Registration of user failed: foo bar">>,
                 list_to_bitstring(rebar3_hex_user:format_error({registration_failure, #{<<"foo">> => <<"bar">>}}))).

format_error_generate_key_test(_Config) ->
    ?assertEqual(<<"Failure generating authentication tokens: eh">>,
                 list_to_bitstring(rebar3_hex_user:format_error({generate_key, <<"eh">>}))).

format_error_passwords_do_not_match_test(_Config) ->
    ?assertEqual(<<"Password confirmation failed. The passwords must match.">>,
                 list_to_bitstring(rebar3_hex_user:format_error(passwords_do_not_match))).

format_error_reset_account_password_test(_Config) ->
    ?assertEqual(<<"Error reseting account password: eh?">>,
                 list_to_bitstring(rebar3_hex_user:format_error({reset_account_password, <<"eh?">>}))).

format_error_bad_command_test(_Config) ->
    ?assertMatch(<<"Invalid arguments, expected one of:", _Rest/binary>>,
                 list_to_bitstring(rebar3_hex_user:format_error(bad_command))).

format_error_input_required_test(_Config) ->
    Result = rebar3_hex_user:format_error({input_required, "Username"}),
    ?assertMatch("The task you are attempting to run requires a Username. " ++ _, lists:flatten(Result)).

format_error_key_revoke_test(_Config) ->
    Result = rebar3_hex_user:format_error({key_revoke, {error, #{<<"status">> => 404}}}),
    ?assertEqual("The key you tried to revoke was not found", lists:flatten(Result)).

format_error_key_revoke_all_test(_Config) ->
    Result = rebar3_hex_user:format_error({key_revoke_all, {error, #{<<"message">> => <<"error msg">>}}}),
    ?assertEqual("Error revoking all keys : error msg", lists:flatten(Result)).

format_error_key_list_test(_Config) ->
    Result = rebar3_hex_user:format_error({key_list, {error, #{<<"message">> => <<"list error">>}}}),
    ?assertEqual("Error listing keys : list error", lists:flatten(Result)).

format_error_auth_failed_test(_Config) ->
    Result = rebar3_hex_user:format_error({auth_failed, timeout}),
    ?assertEqual("Authentication failed: timeout", lists:flatten(Result)).

format_error_revoke_failed_test(_Config) ->
    Result = rebar3_hex_user:format_error({revoke_failed, 500, <<"server error">>}),
    ?assertMatch("Failed to revoke token (500): " ++ _, lists:flatten(Result)).

format_error_unknown_test(_Config) ->
    ?assertEqual("An unknown error was encountered. Run with DIAGNOSTIC=1 for more details.",
                 rebar3_hex_user:format_error('eh?')).
