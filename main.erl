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
    Nodes = createNodeList([], 15),
    timer:sleep(1000),
    register(ca, spawn(?MODULE, ca_code, [Nodes])),
    TxIncluder = spawn(?MODULE, includeTx, [[], Nodes]),
    timer:sleep(1000),
    [X ! {Nodes} || X <- Nodes],
    timer:sleep(500),
    % spawn(?MODULE, automator, [TxIncluder]),
    % timer:sleep(1000),
    
    % Share the group with the nodes so they can multicast.
    ok.

% ----------------------------------
% Spawing Nodes and saving their PIDs in a list
% ----------------------------------
createNodeList(Nodes, 0) ->
    printList("Created Nodes list", Nodes),
    Nodes;

createNodeList(Nodes, Num) when Num > 0 ->
    Pid = spawn(node, node_code, [[{{"God", "CA", 576460752303423488, 0, 576460752303423488}, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}], []]),
    NewList = [Pid | Nodes],
    NumNew = Num - 1,
    createNodeList(NewList, NumNew).

% ----------------------------------
% Helper functions
% ----------------------------------
printList(Message, List) ->
    io:format("~p: ~p~n", [Message, List]).

automator(Recipient) ->
    % Recipient ! {"CA", "pablo", 100},
    % timer:sleep(500),
    % % Recipient ! {"CA", "nina", 50},
    % % timer:sleep(500),
    % % Recipient ! {"CA", "pablo", 30},
    % % timer:sleep(500),
    % % Recipient ! {"CA", "peter", 200},
    % % timer:sleep(500),
    Recipient ! {"CA", "nina", 10},
    timer:sleep(1000),
    Recipient ! {"CA", "paul", 5},
    timer:sleep(1000),
    Recipient ! {"nina", "paul", 5},
    timer:sleep(500).

searchList(Item, []) ->
    false;

searchList(Item, List) ->

    [Entry | R] = List,
    Bool = (Item == Entry),
    case Bool of
        true ->
            true;
        false ->
            searchList(Item, R)
    end.

% ----------------------------------
% Function where the Includer process runs 
% that includes new TXs into the TxPool.
% In also send the oldest Tx to a random Miner
% using a shuffle of the list
% ----------------------------------
includeTx(Pool, Nodes) ->
    receive
        {From, To, Amount} ->
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
            NextMiner ! {From, To, Amount},
            includeTx(T, R);
        true ->
            includeTx(Pool, Nodes)
    end.

ca_code(Clients) ->
    receive
        {register, Pid, SecretName} -> 
            io:format("INFO: ca should hash(~p) and save/check hash in list. send ok or nope~n~n", [SecretName]),
            Pid ! {self(), ok},
            ca_code([SecretName | Clients]);

        {login, Pid, SecretName} -> 
            Bool = searchList(SecretName, Clients),
            case Bool of
                true ->
                    Pid ! {self(), ok};
                false ->
                    Pid ! {self(), nope}
            end,
            ca_code(Clients);

        {Client, To, From, Amount} ->
            io:format("INFO: Now would be sending to miner~n"),
            Client ! {self(), ok},
            ca_code(Clients)
    end.