-module(ca).
-import('lists', [append/2]).
-export([main/0, automator/0,
        includeTx/2, 
        createMinerList/2,
        dummyMining/0,
        printList/2]).

% ----------------------------------
% Main Process that spawns stuff
% ----------------------------------
main() ->
    Miners = createMinerList([], 10),
    register(txIncluder, spawn(?MODULE, includeTx, [[], Miners])),
    spawn(?MODULE, automator, []),
    timer:sleep(10000),
    unregister(txIncluder).

% ----------------------------------
% Spawing Dummy Miners and saving their PIDs in a list
% ----------------------------------
createMinerList(Miners, 0) ->
    printList("Miners", Miners),
    Miners;

createMinerList(Miners, Num) ->
    Pid = spawn(?MODULE, dummyMining, []),
    NewList = [Pid | Miners],
    NumNew = Num - 1,
    createMinerList(NewList, NumNew).


% ----------------------------------
% Helper functions
% ----------------------------------
printList(Message, List) ->
    io:format("~p: ~p~n", [Message, List]).

automator() ->
    txIncluder ! {giorgio, pablo, 100},
    timer:sleep(500),
    txIncluder ! {giorgio, nina, 50},
    timer:sleep(500),
    txIncluder ! {mathias, pablo, 30},
    timer:sleep(500),
    txIncluder ! {giorgio, peter, 200},
    timer:sleep(500),
    txIncluder ! {martin, nina, 10},
    timer:sleep(500),
    txIncluder ! {peter, paul, 5},
    timer:sleep(500).

% ----------------------------------
% Function where the Includer process runs 
% that includes new TXs into the TxPool.
% In also send the oldest Tx to a random Miner
% using a shuffle of the list
% ----------------------------------
includeTx(Pool, Miners) ->
    receive
        {To, From, Amount} when length(Pool) ->
            UpdatedTxPool = append(Pool, [{To, From, Amount}]),
            io:format("From: ~p, To: ~p, Amount: ~p ~n", [From, To, Amount]),
            includeTx(UpdatedTxPool, Miners);

        {printPool} ->
            io:format("TxPool: ~p~n", [Pool])
    end,

    % shuffle list

    % take oldest tx and next Miner
    if 
        length(Pool) > 0 ->
            [{Receiver, Sender, Money} | T] = Pool,
            [NextMiner | R] = Miners,
            NextMiner ! {Receiver, Sender, Money},
            includeTx(T, R);
        true ->
            includeTx(Pool, Miners)
    end.

% ----------------------------------
% Function for the Dummy Miners
% ----------------------------------
dummyMining() ->
    receive
        {To, From, Amount} ->
            io:format("Mined this Tx | From: ~p, To: ~p, Amount: ~p ~n", [From, To, Amount])
    end.