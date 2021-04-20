-module(chat_server_gen_server).

-behaviour(gen_server).

%% -----------------------------------------------------------------------------
%% gen_server
%% -----------------------------------------------------------------------------
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API functions
-export([start_link/0
        ,store_username/2
        ,get_hitchhickers/1
        ,remove_hitchhicker/1
        ,check_bus/1
        ,bus_list/0
        ]).


%% PUBLIC API
%%%===================================================================
%%% API functions
%%%===================================================================



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    lager:info("start_link: ~p~n", [Return]),
    Return.

store_username(BusName, Username) ->
    gen_server:call(?MODULE, {store_username, BusName, Username}).

get_hitchhickers(BusName) ->
    gen_server:call(?MODULE, {get_hitchhickers, BusName}).

remove_hitchhicker(Hitchhicker) ->
    gen_server:call(?MODULE, {remove_hitchhickers, Hitchhicker}).

check_bus(BusName) ->
    gen_server:cast(?MODULE, {check_bus, BusName}).

bus_list() ->
    gen_server:call(?MODULE, {bus_list}).


%% -----------------------------------------------------------------------------
%% gen_server
%% Cfr https://riptutorial.com/erlang/example/24705/using-gen-server-behavior
%% -----------------------------------------------------------------------------

init([]) ->
    lager:info("Chat Server started"),
    sqlite3:open(chat_server, [{file, "/var/lib/chat_server.db"}]),
    % Ensures multiple read access via
    sqlite3:sql_exec(chat_server, "PRAGMA journal_mode = wal"),
    sqlite3:sql_exec(chat_server, "PRAGMA foreign_keys = true"),  
    %% Eval PRAGMA busy_timeout= in milliseconds
    BusSql = <<"CREATE TABLE if not exists bus_list  (
               id INTEGER PRIMARY KEY,
               ts TEXT default (timestamp('now')),
               bus_name TEXT);">>,
    ok=sqlite3:sql_exec(chat_server, BusSql),    
    lager:info("Info... ~p~n",  [ sqlite3:table_info(chat_server,bus_list) ] ),
    State = [],
    Return = {ok, State},
    lager:info("init: ~p~n", [State]),
    Return.

handle_call(Request, From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    lager:info("handle_call: ~p ret: ~p~n", [{Request,From}, Return]),
    Return.

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    lager:info("handle_cast: ~p~n", [Return]),
    Return.

handle_info(_Info, State) ->
    Return = {noreply, State},
    lager:info("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    Return = ok,
    lager:info("terminate: ~p~n", [Return]),
    sqlite3:close(chat_server),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    lager:info("code_change: ~p~n", [Return]),
    Return.