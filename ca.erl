-module(ca).
-export([ca_init/1, includeTx/2]).
-import('lists', [append/2]).
-import('node',[node_code/2]).
-import('helper',[searchList/2]).

% Don't forget to unregister

ca_init(Nodes) ->
    register(txIncluder, spawn(?MODULE, includeTx, [[], Nodes])),
    io:format("Line 9~n"),
    ca_code(Nodes, []),
    ok.

ca_code(Nodes, Clients) ->
    receive
        {register, Pid, SecretName} -> 
           crypto:start(),
            HInfo = "register",
            HashedName = crypto:mac(hmac, sha256, SecretName, HInfo),
            Bool = helper:searchList(HashedName, Clients),
            case Bool of
                false ->
                    Pid ! {self(), ok};
                true ->
                    io:format("Client already exist, please log in"),
                    Pid ! {self(), nope}
            end,
            ca_code(Nodes, [SecretName | Clients]);

        {login, Pid, SecretName} -> 
            crypto:start(),
            HInfo = "login",
            HashedName = crypto:mac(hmac, sha256, SecretName, HInfo),
            Bool = helper:searchList(HashedName, Clients),
            case Bool of
                true ->
                    Pid ! {self(), ok};
                false ->
                    Pid ! {self(), nope}
            end,
            ca_code(Nodes, Clients);

        {Client, From, To, Amount} ->
            txIncluder ! {From, To, Amount},
            io:format("INFO: Now would be sending to miner~n"),
            Client ! {self(), ok},
            ca_code(Nodes, Clients)
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