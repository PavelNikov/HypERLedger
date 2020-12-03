-module(ca).
-export([ca_init/1, includeTx/2]).
-import('lists', [append/2]).
-import('node',[node_code/2]).
-import('helper',[searchList/2, calculatePAddr/1]).

% Don't forget to unregister

ca_init(Nodes) ->
    register(txIncluder, spawn(?MODULE, includeTx, [[], Nodes])),
    ca_code(Nodes, []),
    ok.

ca_code(Nodes, Clients) ->
    receive
        % register request from client application
        {register, Pid, SecretName} -> 
            HashedName = helper:calculatePAddr(SecretName),
            Bool = helper:searchList(HashedName, Clients),
            case Bool of
                false ->
                    TxIncluder = whereis(txIncluder),
                    TxIncluder ! {self(), "0x494A64075CBEDAEE8C4DE3D13D5ED2DAC4FAC9A25DD62B7853F953D7473A9326", HashedName, 500},
                    receive
                        {TxIncluder, ok, _} ->
                            Pid ! {self(), ok};
                        {TxIncluder, nope, _} ->
                            Pid ! {self(), nope}
                    end;
                true ->
                    io:format("Client already exist, please log in instead~n"),
                    Pid ! {self(), nope}
            end,
            ca_code(Nodes, [HashedName | Clients]);

        % login request from client application
        {login, Pid, SecretName} -> 
            HexName = helper:calculatePAddr(SecretName),
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
            HashedFrom = helper:calculatePAddr(From),
            % Send Tx with public addresses to TxPool
            TxIncluder = whereis(txIncluder),
            TxIncluder ! {self(), HashedFrom, To, Amount},
            receive
                {TxIncluder, ok, Message} ->
                    Client ! {self(), ok, Message};
                {TxIncluder, nope, Message} ->
                    Client ! {self(), nope, Message}
            end,
            ca_code(Nodes, Clients);

        % Request to retreive Public Address from client application
        {Client, retrievePAddr, SecretName} ->
            PAddr = helper:calculatePAddr(SecretName),
            Client ! {self(), PAddr},
            ca_code(Nodes, Clients);

        % Request to retreive client account balance
        {Client, retrieveBalance, SecretName} ->
            PublicAddr = helper:calculatePAddr(SecretName),
            [Node | _] = Nodes,
            Node ! {self(), retrieveBalance, PublicAddr},
            receive 
                {Node, ok, Balance} ->
                    Client ! {self(), ok, Balance};
                {Node, nope} ->
                    Client ! {self(), nope}
            end,
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
            io:format("From: ~s~nTo: ~s~nAmount: ~p~n", [From, To, Amount]),
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
    [{From, To, Amount} | T] = Pool,
    [NextMiner | _] = Nodes,
    NextMiner ! {self(), From, To, Amount},
    receive
        {NextMiner, ok} ->
            Message = "Sender has enough funds.",
            Ca ! {self(), ok, Message};
        {NextMiner, nope} ->
            Message = "Sender does not have enough funds.",
            Ca ! {self(), nope, Message}
    end,
    includeTx(T, Nodes).