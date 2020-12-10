-module(ca).
-export([ca_init/1, includeTx/2]).
-import('lists', [append/2]).
-import('node',[node_code/2]).
-import('helper',[searchList/2, calculatePAddr/1]).
-import('global', [register_name/2, whereis_name/1]).


% ====================================================================================================== %
%                                   Central Authority of the blockchain                                  %
%                                        that serves as intermediary                                     %
%                                    between the clients and the nodes                                   %
% ====================================================================================================== %


% ===================================
% Initialize ca with a group of Nodes
% (Called in main.erl)
% ===================================
ca_init(Nodes) ->
    register(txIncluder, spawn_link(?MODULE, includeTx, [[], Nodes])),
    loop(Nodes, []),
    ok.


% ===================================
%  
% ===================================
loop(Nodes, Clients) ->
    receive
        % register request from client application
        {client, Host, register, SecretName} -> 
            HashedName = helper:calculatePAddr(SecretName),
            Bool = helper:searchList(HashedName, Clients),
            case Bool of
                false ->
                    txIncluder ! {self(), "a494A64075CBEDAEE8C4DE3D13D5ED2DAC4FAC9A25DD62B7853F953D7473A9326", HashedName, 500},
                    TxIncluder = whereis(txIncluder),
                    receive
                        {TxIncluder, ok, _} ->
                            {client, Host} ! {ca, ok};
                        {TxIncluder, nope, _} ->
                            {client, Host}! {ca, nope}
                        after 2000 ->
                            timeout
                    end,
                    loop(Nodes, [HashedName|Clients]);
                true ->
                    M = "Client already exist, please log in instead~n",
                    {client, Host} ! {ca, nope, M},
                    loop(Nodes, Clients)
                    
            end;

        % login request from client application
        {client, Host, login, SecretName} -> 
            HexName = helper:calculatePAddr(SecretName),
            Bool = helper:searchList(HexName, Clients),
            case Bool of
                true ->
                    {client, Host} ! {ca, ok};
                false ->
                    {client, Host}! {ca, nope}
            end,
            loop(Nodes, Clients);

        % new transaction to include into Pool from client application
        {client, Host, From, To, Amount} ->
            HashedFrom = helper:calculatePAddr(From),
            % Send Tx with public addresses to TxPool
            txIncluder ! {self(), HashedFrom, atom_to_list(To), Amount},
            TxIncluder = whereis(txIncluder),
            receive
                {TxIncluder, ok, Message} ->
                    {client, Host} ! {ca, ok, Message};
                {TxIncluder, nope, Message} ->
                    {client, Host} ! {ca, nope, Message}
                after 2000 ->
                    timeout
            end,
            loop(Nodes, Clients);

        % Request to retreive Public Address from client application
        {client, Host, retrievePAddr, SecretName} ->
            PAddr = helper:calculatePAddr(SecretName),
            {client, Host} ! {ca, PAddr},
            loop(Nodes, Clients);

        % Request to retreive client account balance
        {client, Host, retrieveBalance, SecretName} ->
            PublicAddr = helper:calculatePAddr(SecretName),
            ShuffledNodes = shuffleList(Nodes),
            [Node | _] = ShuffledNodes,
            Node ! {self(), retrieveBalance, PublicAddr},
            receive 
                {Node, ok, Balance} ->
                    {client, Host} ! {ca, ok, Balance};
                {Node, nope} ->
                    {client, Host} ! {ca, nope}
            end,
            loop(ShuffledNodes, Clients);

        % Request to send complete ledger
        {client, Host, printChain} ->
            ShuffledNodes = shuffleList(Nodes),
            [Node | _] = ShuffledNodes,
            Node ! {self(), sendLedger},
            receive
                {Node, ok, Ledger} ->
                    {client, Host} ! {ca, ok, Ledger}
                after 2000 ->
                    timeout
            end,
            loop(ShuffledNodes, Clients)
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
