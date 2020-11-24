-module(main).
-import('lists', [append/2]).
-import('node',[node_code/2]).
-export([init/0, automator/1,
        includeTx/2, 
        createNodeList/2,
        printList/2,
        shuffleList/1,
        sendToMiner/5]).

% ----------------------------------
% Main Process that spawns stuff
% ----------------------------------
init() ->
    Nodes = createNodeList([], 10),
    timer:sleep(1000),
    TxIncluder = spawn(?MODULE, includeTx, [[], Nodes]),
    timer:sleep(1000),
    spawn(?MODULE, automator, [TxIncluder]),
    timer:sleep(1000),
    
    % Share the group with the nodes so they can multicast.
    [X ! {Nodes} || X <- Nodes],
    ok.

% ----------------------------------
% Spawing Nodes and saving their PIDs in a list
% ----------------------------------
createNodeList(Nodes, 0) ->
    printList("Created Nodes list", Nodes),
    Nodes;

createNodeList(Nodes, Num) when Num > 0 ->
    Pid = spawn(node, node_code, [[{{"None", "None", "None", "None", "None"}, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}], []]),
    NewList = [Pid | Nodes],
    NumNew = Num - 1,
    createNodeList(NewList, NumNew).

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
includeTx(Pool, Nodes) ->
    receive
        {To, From, Amount} ->
            UpdatedTxPool = append(Pool, [{From, To, Amount}]),
            io:format("From: ~p, To: ~p, Amount: ~p ~n", [From, To, Amount]),
            ShuffledNodes = shuffleList(Nodes),
            sendToMiner(UpdatedTxPool, ShuffledNodes, From, To, Amount);

        {printPool} ->
            io:format("TxPool: ~p~n", [Pool])
    end.

% shuffle list
shuffleList(List) ->
    [X||{_,X} <- lists:sort([{rand:uniform(), Miner} || Miner <- List])].

% take oldest tx and next Miner
sendToMiner(Pool, Nodes, From, To, Amount) ->
    if 
        length(Pool) > 0 ->
            [{From, To, Amount} | T] = Pool,
            [NextMiner | R] = Nodes,
            NextMiner ! {{From, To, Amount}},
            includeTx(T, R);
        true ->
            includeTx(Pool, Nodes)
    end.