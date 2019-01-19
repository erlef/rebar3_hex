-module(hex_db).

-behaviour(gen_server).

-export([start_link/0, stop/1]).

-export([add_user/1]).

-export([handle_call/3, handle_info/2, handle_cast/2, init/1]).

-export([terminate/2]).

% Client
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

add_user(#{<<"username">> := Username, <<"email">> := Email} = User) ->
    case ets:insert_new(?MODULE, {Username, User})  of 
        true -> 
            case ets:insert_new(?MODULE, {Email, Username}) of 
                true -> 
                    {ok, Username};
                false ->
                    ets:delete(?MODULE, {Username, User}),
                    {error, email_exists}
            end;
        false -> {error, user_exists}
    end.

% Server
init([]) ->
    _Tid = ets:new(?MODULE, [named_table, public, {write_concurrency, true}]),
    {ok, {}}.

handle_call(_, State, _) -> 
    {ok, State}.

handle_cast(_, State) -> 
    {ok, State}.

handle_info(_, State) -> 
    {ok, State}. 

terminate(_Reason, _State) ->
    ok.
