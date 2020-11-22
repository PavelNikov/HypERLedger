-module(ca).
-import('lists', [append/2]).
-import('string', [join/2]).
-export([main/0, automator/1,
        includeTx/2, 
        createMinerList/2,
        dummyMining/0,
        printList/2,
        shuffleList/1,
        sendToMiner/5]).

% ----------------------------------
% Main Process that spawns stuff
% ----------------------------------
main() ->
    Miners = createMinerList([], 10),
    timer:sleep(1000),
    TxIncluder = spawn(?MODULE, includeTx, [[], Miners]),
    timer:sleep(1000),
    spawn(?MODULE, automator, [TxIncluder]),
    timer:sleep(1000).
% ----------------------------------
% Spawing Dummy Miners and saving their PIDs in a list
% ----------------------------------
createMinerList(Miners, 0) ->
    printList("Created Miner list", Miners),
    Miners;

createMinerList(Miners, Num) when Num > 0 ->
    Pid = spawn(?MODULE, dummyMining, []),
    NewList = [Pid | Miners],
    NumNew = Num - 1,
    createMinerList(NewList, NumNew).


% ----------------------------------
% Helper functions
% ----------------------------------
printList(Message, List) ->
    io:format("~p: ~p~n", [Message, List]).

automator(Recipient) ->
    Recipient ! {giorgio, pablo, 100},
    timer:sleep(500),
    Recipient ! {giorgio, nina, 50},
    timer:sleep(500),
    Recipient ! {mathias, pablo, 30},
    timer:sleep(500),
    Recipient ! {giorgio, peter, 200},
    timer:sleep(500),
    Recipient ! {martin, nina, 10},
    timer:sleep(500),
    Recipient ! {peter, paul, 5},
    timer:sleep(500).

% ----------------------------------
% Function where the Includer process runs 
% that includes new TXs into the TxPool.
% In also send the oldest Tx to a random Miner
% using a shuffle of the list
% ----------------------------------
includeTx(Pool, Miners) ->
    receive
        {To, From, Amount} ->
            UpdatedTxPool = append(Pool, [{To, From, Amount}]),
            io:format("From: ~p, To: ~p, Amount: ~p ~n", [From, To, Amount]),
            ShuffledMiners = shuffleList(Miners),
            sendToMiner(UpdatedTxPool, ShuffledMiners, To, From, Amount);

        {printPool} ->
            io:format("TxPool: ~p~n", [Pool])
    end.

% shuffle list
shuffleList(List) ->
    [X||{_,X} <- lists:sort([{rand:uniform(), Miner} || Miner <- List])].

% take oldest tx and next Miner
sendToMiner(Pool, Miners, Receiver, Sender, Money) ->
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
            io:format("Mined this Tx | From: ~p, To: ~p, Amount: ~p ~n", [From, To, Amount]),
            dummyMining()
    end.