-module(chat_bus_test).

-include_lib("eunit/include/eunit.hrl").

-define(FuncTest(Name), {??Name, fun Name/0}).

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [  ?FuncTest(bus_list)         
       ,?FuncTest(new_user)
     ]}.

setup()->
    % Ensure entire application under test is started
    % The tree is app->sup->chat_bus
    % see leptus_http_SUITE for an example
    application:ensure_started(chat_server),
    ok.

teardown({ok,_Pid}) ->
    %% Consider if we really need to stop the chat server
    %% because this can slow down tests
    %$ application:stop(chat_server),
    ok;
teardown(_)->
    ok.

bus_list()->    
    {ok, ListOfBuses}=chat_bus:bus_list(),
    % At least the hippy bus must be here
    ?assertEqual(true,lists:member("hippy",ListOfBuses)).
new_user() ->
    {ok, Username}=chat_bus:store_username("hippy","Bob"),
    ?assertEqual("Bob",Username).