%% Vendored from hex_core v0.6.8, do not edit manually

-module(r3h_hex_api_auth).
-export([test/2]).

%% @doc
%% Test an auth key against a given domain and resource.
%%
%% Examples:
%%
%% ```
%% 1> Params = #{domain => <<"repository">>, resource => <<"gustafson_motors">>}.
%% 2> r3h_hex_api_auth:test_key(r3h_hex_core:default_config(), Params).
%% {ok,{204, ..., nil}}
%% '''
%% @end
-spec test(r3h_hex_core:config(), map()) -> r3h_hex_api:response().
test(Config, #{domain := Domain, resource := Resource}) ->
    URI = ["auth", "?domain=", Domain, "&resource=", Resource],
    r3h_hex_api:get(Config, list_to_binary(URI)).
