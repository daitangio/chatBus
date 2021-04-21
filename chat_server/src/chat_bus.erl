-module(chat_bus).

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


%% Private level api

% API Migration is an recursive function....
migrate_db(3) ->
    % Version 2  is the next one you will develop
    lager:info("Database on sync");

migrate_db(2)->
    Hitchhickers = <<" Create Table if not exists USER (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        name text not null,
                        current_bus default 1,
                        CONSTRAINT FK_BUS
                          FOREIGN KEY (CURRENT_BUS)
                          REFERENCES BUS_LIST(ID)
                    );
        ">>,
    ok=sqlite3:sql_exec(chat_server, Hitchhickers),
    ok=sqlite3:sql_exec(chat_server,"UPDATE bus_config SET value=? WHERE key='db_version'",[{1,2}]),
    migrate_db(3);

migrate_db(1) ->
    lager:info("Migrating to Version schema 1"),
    % Ensure hippy bus is here
    sqlite3:write(chat_server, bus_list, [{id,1}, {bus_name, "hippy"}]),
    % Go..
    ok=sqlite3:sql_exec(chat_server,"UPDATE bus_config SET value=? WHERE key='db_version'",[{1,1}]),
    migrate_db(2);
    
migrate_db(0) ->
    %% Eval PRAGMA busy_timeout= in milliseconds
    BusSql = <<"CREATE TABLE if not exists bus_list  (
               id INTEGER PRIMARY KEY AUTOINCREMENT,
               ts TEXT DEFAULT CURRENT_TIMESTAMP,
               bus_name TEXT);">>,    
    ok=sqlite3:sql_exec(chat_server, BusSql),   
    {rowid,_} = sqlite3:write(chat_server, bus_config, [{key,"db_version"},{value,0}]),    
    migrate_db(1).

ensure_db_migrated()->
    sqlite3:sql_exec(chat_server, "PRAGMA journal_mode = wal"),
    sqlite3:sql_exec(chat_server, "PRAGMA foreign_keys = true"),  
    SchemaMgm = <<"Create table if not exists bus_config (
                    key text not null primary key,
                    value text not null);">>,
    ok=sqlite3:sql_exec(chat_server, SchemaMgm),      
    %% check schema version
    [{columns, ["value"]}, {rows, DBVersion}]=sqlite3:sql_exec(chat_server,"select value from bus_config where key='db_version';"),
    if DBVersion =:= []  -> migrate_db(0);       
       % Migrate to max
       true -> 
           {X}=lists:nth(1,DBVersion),
           {CurrentDBVersion,_Rest} = string:to_integer(X),
           migrate_db(CurrentDBVersion+1)
    end,
    lager:info("Info... ~p~n",  [ sqlite3:table_info(chat_server,bus_list) ] ).


%% -----------------------------------------------------------------------------
%% gen_server
%% Cfr https://riptutorial.com/erlang/example/24705/using-gen-server-behavior
%% -----------------------------------------------------------------------------

init([]) ->
    lager:info("Chat Server started"),
    sqlite3:open(chat_server, [{file, "/var/lib/chat_server.db"}]),
    % Ensures multiple read access via
    ensure_db_migrated(),
    State = [],
    Return = {ok, State},
    lager:info("init: ~p~n", [State]),
    Return.

handle_call({bus_list}, _From, State) ->
    [{columns, ["bus_name"]}, {rows, AllRows}]=sqlite3:sql_exec(chat_server, "select bus_name from bus_list order by ts;"),
    % AllRows i,e. [{<<"hippy">>},{<<"h2">>} ,...... ]
    % I now this code is terrible
    % I hope Erlang Gods will forgive me:
    ListOfBinary= [ X || {X} <- AllRows],
    ListOfBus=lists:map(fun binary:bin_to_list/1,ListOfBinary),
    {reply, {ok, ListOfBus}, State};



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