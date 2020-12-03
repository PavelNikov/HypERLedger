-module(ca).
-export([ca_init/1, includeTx/2]).
-import('lists', [append/2]).
-import('node',[node_code/2]).
-import('helper',[searchList/2, binaryToHex/1]).

% Don't forget to unregister

ca_init(Nodes) ->
    register(txIncluder, spawn(?MODULE, includeTx, [[], Nodes])),
    ca_code(Nodes, []),
    ok.

ca_code(Nodes, Clients) ->
    receive
        % register request from client application
        {register, Pid, SecretName} -> 
            crypto:start(),
            HashedName =  helper:binaryToHex(crypto:mac(hmac, sha256, SecretName, "security")),
            Bool = helper:searchList(HashedName, Clients),
            case Bool of
                false ->
                    Pid ! {self(), ok};
                true ->
                    io:format("Client already exist, please log in instead~n"),
                    Pid ! {self(), nope}
            end,
            ca_code(Nodes, [HashedName | Clients]);

        % login request from client application
        {login, Pid, SecretName} -> 
            crypto:start(),
            HexName = helper:binaryToHex(crypto:mac(hmac, sha256, SecretName, "security")),
            Bool = helper:searchList(HexName, Clients),
            case Bool of
                true ->
                    Pid ! {self(), ok};
                false ->
                    Pid ! {self(), nope}
            end,
            ca_code(Nodes, Clients);

        % new transaction to include into Pool from client application
        {Client, From, To, Amount} ->
            crypto:start(),
            HashedFrom = helper:binaryToHex(crypto:mac(hmac, sha256, atom_to_list(From), "security")),
            % Send Tx with puublic addresses to TxPool
            txIncluder ! {self(), HashedFrom, To, Amount},
            receive
                {txIncluder, ok, Message} ->
                    Client ! {self(), ok, Message};
                {txIncluder, nope, Message} ->
                    Client ! {self(), nope, Message}
            end,
            ca_code(Nodes, Clients);

        % Request to retreive Public Address from client application
        {Client, retrievePAddr, SecretName} ->
            crypto:start(),
            PAddr = helper:binaryToHex(crypto:mac(hmac, sha256, atom_to_list(SecretName), "security")),
            Client ! {self(), PAddr},
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
        {Ca, From, To, Amount} ->
            UpdatedTxPool = append(Pool, [{From, To, Amount}]),
            io:format("From: ~p, To: ~p, Amount: ~p ~n", [From, To, Amount]),
            ShuffledNodes = shuffleList(Nodes),
            sendToMiner(Ca, UpdatedTxPool, ShuffledNodes, From, To, Amount);

        {printPool} ->
            io:format("TxPool: ~p~n", [Pool])
    end.

% shuffle list
shuffleList(List) ->
    [X||{_,X} <- lists:sort([{rand:uniform(), Miner} || Miner <- List])].

% take oldest tx and next Miner
sendToMiner(Ca, Pool, Nodes, From, To, Amount) ->
    if 
        length(Pool) > 0 ->
            [{From, To, Amount} | T] = Pool,
            [NextMiner | R] = Nodes,
            NextMiner ! {From, To, Amount},
            receive
                {NextMiner, ok} ->
                    Message = io:format("Sender has enough funds.~n"),
                    Ca ! {self(), ok, Message};
                {NextMiner, nope} ->
                    Message = io:format("Sender does not have enough funds.~n"),
                    Ca ! {self(), nope, Message}
            end,
            includeTx(T, R);
        true ->
            includeTx(Pool, Nodes)
    end.