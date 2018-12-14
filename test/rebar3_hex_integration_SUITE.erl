-module(rebar3_hex_integration_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CONFIG, maps:merge(hex_core:default_config(), #{
                             name        => <<"foo">>,
                             repo        => <<"foo">>, 
                             api_url     => <<"http://127.0.0.1:3000">>,
                             repo_url    => <<"http://127.0.0.1:3000">>,
                             repo_verify => false})).

%%%%%%%%%%%%%%%%%%
%%%  CT hooks  %%%
%%%%%%%%%%%%%%%%%%

all() ->
[sanity_check, register_user, register_existing_user].

init_per_suite(Config) -> 
    meck:new([ec_talk, rebar3_hex_utils], [passthrough, no_link]),
    Config.

end_per_suite(Config) ->
  meck:unload([ec_talk, rebar3_hex_utils]),
  Config.

init_per_testcase(_Tc, Cfg) -> 
    {ok, StorePid} = hex_db:start_link(),
    {ok, MockPid} = elli:start_link([{callback, hex_api_callback}, {port, 3000}]),
    [{hex_store, StorePid}, {hex_mock_server, MockPid} | Cfg].

end_per_testcase(_Tc, Config) -> 
    StorePid = ?config(hex_store, Config),
    MockPid = ?config(hex_mock_server, Config),
    ok = hex_db:stop(StorePid),
    ok = elli:stop(MockPid),
    reset_mocks([rebar3_hex_utils, ec_talk]),
    Config.

%%%%%%%%%%%%%%%%%%
%%% Test Cases %%%
%%%%%%%%%%%%%%%%%%

sanity_check(_Config) -> 
    begin
      User = <<"mr_pockets">>,
      Pass = <<"special_shoes">>,
      Email = <<"foo@bar.baz">>,
      {ok, {201, _Headers, Res}} = create_user(User, Pass, Email),
      ?assertEqual(User, maps:get(<<"username">>, Res)),
      ?assertEqual(Email, maps:get(<<"email">>, Res)),
      ?assert(maps:is_key(<<"inserted_at">>, Res)),
      ?assert(maps:is_key(<<"updated_at">>, Res))
    end.


register_user(_Config) ->
     begin
         mock_register_io(<<"mr_pockets">>, <<"foo@bar.org">>, <<"special_shoes">>),
         State = rebar_state:new(), 
         Exp = {ok, State},
         ?assertMatch(Exp, rebar3_hex_user:hex_register(config(), State))
     end.

register_existing_user(_Config) ->
     begin
         State = rebar_state:new(),
         User = <<"mr_pockets">>,
         Pass = <<"special_shoes1">>,
         Email = <<"foo@bar.org">>,
         create_user(User, Pass, Email),
         mock_register_io(<<"mr_pockets1">>, Email, Pass),
         ExpReason1 = <<"email already in use">>,
         ExpErr1 = {error, {rebar3_hex_user, {registration_failure, ExpReason1}}},
         ?assertMatch(ExpErr1, rebar3_hex_user:hex_register(config(), State))
     end.


%%%%%%%%%%%%%%%%%%
%%%  Helpers   %%%
%%%%%%%%%%%%%%%%%%

config() -> 
    ?CONFIG.

create_user(User, Pass, Email) -> 
    hex_api_user:create(config(), User, Pass, Email).

mock_register_io(Username, Email, Password) -> 
    ExpectedInfo = "By registering an account on Hex.pm you accept all our"
         ++ " policies and terms of service found at https://hex.pm/policies\n", 
    meck:expect(rebar3_hex_utils, get_password, fun(_Arg) -> Password end),
    meck:expect(ec_talk, say, fun(Arg) -> 
        case Arg of 
            ExpectedInfo -> 
                ok;
            "Registering..." ->
                ok
        end        
    end),
    meck:expect(ec_talk, ask_default, fun(Prompt, string, "") ->
        case Prompt of 
            "Email:" ->
                binary_to_list(Email);
            "Username:" ->
                binary_to_list(Username)
        end
    end).

reset_mocks(Modules) ->
    meck:reset(Modules),
    [meck:delete(Module, Fun, Arity, false)
    || {Module, Fun, Arity} <- meck:expects(Modules, true)].
