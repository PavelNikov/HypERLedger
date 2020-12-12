-module(node_d).
-import(crypto,[start/0, hmac/3, mac/4]).
-import(lists, [member/2]).
-import(string, [join/2]).
-export([init/1, node_code/2, createNodeList/2]).


init(Ca_Host) ->
    io:format("Master Node initialized~n"),
    Nodes = createNodeList([], 15),
    {supervisor, Ca_Host} ! {node, Nodes},
    io:format("Sent Nodes to ca~n"),
    [X ! {master, Nodes} || X <- Nodes],
    io:format("Sent message to all nodes~n"),
    loop().

loop() ->
    timer:sleep(10000),
    loop().

% ----------------------------------
% Spawing Nodes and saving their PIDs in a list
% Give them all the same genesis block  
% ----------------------------------
createNodeList(Nodes, 0) ->
    Nodes;

createNodeList(Nodes, Num) when Num > 0 ->
    Pid = spawn(?MODULE, node_code, [[{{"a52DF85A2088B1088AED8CA38A08515F437642CA65BA0BC52637C1CC53DD67EF8", "a494A64075CBEDAEE8C4DE3D13D5ED2DAC4FAC9A25DD62B7853F953D7473A9326", 576460752303423488, 0, 576460752303423488}, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}],[]]),
    register(list_to_atom(lists:flatten(io_lib:format("node~p", [Num]))), Pid),
    % Add a new entry to the list the name of the node and a number
    NewList = [{list_to_atom(lists:flatten(io_lib:format("node~p", [Num]))), node()} | Nodes],
    NumNew = Num - 1,
    io:format("Done~n"),
    createNodeList(NewList, NumNew).

% ======================================
% Loop for each node                    
% ======================================
node_code(Ledger, Group) ->
    receive
        % Normal Mode:
        {Pid, {From, To, Amount, NSFrom, NSTo}, NHash} when (Pid /= self())->
            % Retrieve the old hash value:
            [{_, OHash}|_] = Ledger,

            % Calculate the Hash to compare with NHash.
            crypto:start(),
            Item = io_lib:format("~s~s~w~w~w",[From, To, Amount, NSFrom, NSTo]),
            Hash = crypto:mac(hmac, sha256, OHash, Item),
            
            % Check whether NHash and Hash match. If they do, update ledger. Otherwise refuse to update:
            Bool = (Hash == NHash),
            case Bool of
                true -> 
                    io:format("The hash values match. Ledger is updated. ~p~n",[self()]),
                    node_code([{{From, To, Amount, NSFrom, NSTo}, NHash}|Ledger], Group);
                false -> 
                    io:format("The hash values don't match. Transaction is rejected. ~p~n",[self()]),
                    node_code(Ledger, Group)
            end;
        
        % Miner Mode:
        {txIncluder, Host, From, To, Amount} ->
            SenderBalance = search_user_current_balance(From, Ledger),
            io:format("Sender Balance before tx: ~p~n", [SenderBalance]),
            ReceiverBalance = search_user_current_balance(To, Ledger),
            io:format("Receiver Balance before tx: ~p~n", [ReceiverBalance]),
            NSFrom = SenderBalance - Amount,
            NSTo = ReceiverBalance + Amount,
            Bool = (SenderBalance >= Amount),
            case Bool of
                true ->
                    {txIncluder, Host} ! {node, ok},
                    % Calculate new Hash
                    [{_, OHash}|_] = Ledger,
                    Item = io_lib:format("~s~s~w~w~w",[From, To, Amount, NSFrom, NSTo]),
                    NHash = crypto:mac(hmac, sha256, OHash, Item),
                    % Multicast the block
                    [X ! {self(), {From, To, Amount, NSFrom, NSTo}, NHash}|| X <- Group, X =/= self()],
                    node_code([{{From, To, Amount, NSFrom, NSTo}, NHash}|Ledger], Group);
    
                false ->
                    {txIncluder, Host} ! {node, nope},
                    node_code(Ledger, Group)
            end;
            
        % Node receives the list of peer nodes to be able to multicast later on.
        {master, List_of_Nodes} ->
            node_code(Ledger, List_of_Nodes);
        
        % retrieve balance and send ok if it is a number
        {ca, Ca_Host, retrieveBalance, PublicAddr} ->
            Balance = search_user_current_balance(PublicAddr, Ledger),
            {ca, Ca_Host} ! {node, ok, Balance},
            node_code(Ledger, Group);
        
        % Send Ledger
        {ca, Ca_Host, sendLedger} ->
            {ca, Ca_Host} ! {node, ok, Ledger},
            node_code(Ledger, Group)
    end.


% ======================================
% Search a users current balance
% ======================================
search_user_current_balance(_, []) ->
    0;
search_user_current_balance(User, [First_Block|Tail]) ->
    {{Sender,Receiver,_,SenderBalance,ReceiverBalance},_} = First_Block,
    Bool1 = (Sender == User),
    case Bool1 of
        true -> SenderBalance;
        false -> 
            Bool2 = (Receiver == User),
            case Bool2 of
                true -> ReceiverBalance;
                false -> search_user_current_balance(User, Tail)
            end
    end.
