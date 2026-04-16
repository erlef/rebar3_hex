-module(rebar3_hex_organization_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
    [
     %% format_error tests
     format_error_no_repo_test,
     format_error_auth_no_key_test,
     format_error_auth_binary_test,
     format_error_auth_map_test,
     format_error_generate_key_binary_test,
     format_error_generate_key_map_test,
     format_error_key_generate_binary_test,
     format_error_key_generate_map_test,
     format_error_key_revoke_all_binary_test,
     format_error_key_revoke_all_map_test,
     format_error_key_list_binary_test,
     format_error_key_list_map_test,
     format_error_bad_command_test,
     format_error_not_a_valid_repo_name_test,
     format_error_get_parent_repo_and_org_name_test,
     format_error_get_repo_by_name_test,
     format_error_unknown_test
    ].

%%%%%%%%%%%%%%%%%%
%%% Test Cases %%%
%%%%%%%%%%%%%%%%%%

%% format_error tests for organization auth/deauth related errors

format_error_no_repo_test(_Config) ->
    Result = rebar3_hex_organization:format_error(no_repo),
    ?assertEqual("Authenticate and generate commands require repository name as argument", Result).

format_error_auth_no_key_test(_Config) ->
    Result = rebar3_hex_organization:format_error(auth_no_key),
    ?assertEqual("Repo authenticate command requires key", Result).

format_error_auth_binary_test(_Config) ->
    Result = rebar3_hex_organization:format_error({auth, <<"authentication error">>}),
    ?assertEqual("Error authenticating organization : authentication error", lists:flatten(Result)).

format_error_auth_map_test(_Config) ->
    Result = rebar3_hex_organization:format_error({auth, #{<<"error">> => <<"bad credentials">>}}),
    ?assertMatch("Error authenticating organization : " ++ _, lists:flatten(Result)).

format_error_generate_key_binary_test(_Config) ->
    Result = rebar3_hex_organization:format_error({generate_key, <<"key generation failed">>}),
    ?assertEqual("Error generating organization key: key generation failed", lists:flatten(Result)).

format_error_generate_key_map_test(_Config) ->
    Result = rebar3_hex_organization:format_error({generate_key, #{<<"error">> => <<"invalid permissions">>}}),
    ?assertMatch("Error generating organization key: " ++ _, lists:flatten(Result)).

format_error_key_generate_binary_test(_Config) ->
    Result = rebar3_hex_organization:format_error({key_generate, <<"key generate error">>}),
    ?assertEqual("Error generating organization key: key generate error", lists:flatten(Result)).

format_error_key_generate_map_test(_Config) ->
    Result = rebar3_hex_organization:format_error({key_generate, #{<<"error">> => <<"map error">>}}),
    ?assertMatch("Error generating organization key: " ++ _, lists:flatten(Result)).

format_error_key_revoke_all_binary_test(_Config) ->
    Result = rebar3_hex_organization:format_error({key_revoke_all, <<"revoke all failed">>}),
    ?assertEqual("Error revoking all organization keys: revoke all failed", lists:flatten(Result)).

format_error_key_revoke_all_map_test(_Config) ->
    Result = rebar3_hex_organization:format_error({key_revoke_all, #{<<"error">> => <<"revoke error">>}}),
    ?assertMatch("Error revoking all organization keys: " ++ _, lists:flatten(Result)).

format_error_key_list_binary_test(_Config) ->
    Result = rebar3_hex_organization:format_error({key_list, <<"list failed">>}),
    ?assertEqual("Error listing organization keys: list failed", lists:flatten(Result)).

format_error_key_list_map_test(_Config) ->
    Result = rebar3_hex_organization:format_error({key_list, #{<<"error">> => <<"list error">>}}),
    ?assertMatch("Error listing organization keys: " ++ _, lists:flatten(Result)).

format_error_bad_command_test(_Config) ->
    Result = rebar3_hex_organization:format_error(bad_command),
    ?assertMatch("Invalid arguments, expected one of:" ++ _, lists:flatten(Result)).

format_error_not_a_valid_repo_name_test(_Config) ->
    Result = rebar3_hex_organization:format_error(not_a_valid_repo_name),
    Expected = "Invalid organization repository: organization name arguments must be given as a fully qualified "
               "repository name (i.e, hexpm:my_org)",
    ?assertEqual(Expected, Result).

format_error_get_parent_repo_and_org_name_test(_Config) ->
    Result = rebar3_hex_organization:format_error({get_parent_repo_and_org_name, {error, not_found}, <<"hexpm:myorg">>}),
    ?assertMatch("Error getting the parent repo for hexpm:myorg" ++ _, lists:flatten(Result)).

format_error_get_repo_by_name_test(_Config) ->
    Result = rebar3_hex_organization:format_error({get_repo_by_name, {error, {not_valid_repo, <<"hexpm">>}}}),
    ?assertMatch("You do not appear to be authenticated as a user to the hexpm repository." ++ _, lists:flatten(Result)).

format_error_unknown_test(_Config) ->
    Result = rebar3_hex_organization:format_error(unknown_error),
    ?assertEqual("An unknown error was encountered. Run with DIAGNOSTIC=1 for more details.", Result).