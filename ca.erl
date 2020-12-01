-module(ca).
-export([ca_code/1]).
-import('lists', [append/2]).
-import('node',[node_code/2]).
-import('helper',[searchList/2]).

ca_code(Clients) ->
    
    TxIncluder = spawn(?MODULE, includeTx, [[], Clients]),
    
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