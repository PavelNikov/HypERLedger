-module(test).
-export([test/0,test_code/0]).


test() ->
    register(pid, spawn(?MODULE, test_code, [])),
    pid ! {hello},
    ok.

test_code() ->
    receive
        {hello} ->
            io:format("~p~n",[self()])
    end,
    test_code().