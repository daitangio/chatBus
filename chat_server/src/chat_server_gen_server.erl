-module(chat_server_gen_server).

-behaviour(gen_server).

%% -----------------------------------------------------------------------------
%% gen_server
%% -----------------------------------------------------------------------------
-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% -----------------------------------------------------------------------------
%% gen_server
%% Cfr https://riptutorial.com/erlang/example/24705/using-gen-server-behavior
%% -----------------------------------------------------------------------------
start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    lager:info("start_link: ~p~n", [Return]),
    Return.

init([]) ->
    lager:info("Chat Server started"),
    State = [],
    Return = {ok, State},
    lager:info("init: ~p~n", [State]),
    Return.

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    lager:info("handle_call: ~p~n", [Return]),
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
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    lager:info("code_change: ~p~n", [Return]),
    Return.