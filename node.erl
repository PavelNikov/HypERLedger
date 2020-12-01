-module(node).
-import(crypto,[start/0, hmac/3, mac/4]).
-import(lists, [member/2]).
-import(string, [join/2]).
-export([main/0,node_code/2]).

node_code(Ledger, Group) ->
    % io:format("~p~n", [Ledger]),

    receive
        % Normal Mode:
        {{From, To, Amount, NSFrom, NSTo}, NHash} -> 
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
                    % io:format("~p~n", [Hash]),
                    node_code([{{From, To, Amount, NSFrom, NSTo}, NHash}|Ledger], Group);
                false -> 
                    io:format("The hash values don't match. Transaction is rejected. ~p~n",[self()]),
                    % io:format("~p~n", [Hash]),
                    node_code(Ledger, Group)
            end;

        % Miner Mode:
        {From, To, Amount} ->
            SenderBalance = search_sender_current_balance(From, Ledger),
            io:format("~p~n", [SenderBalance]),
            ReceiverBalance = search_receiver_current_balance(To, Ledger),
            io:format("~p~n", [ReceiverBalance]),
            NSFrom = SenderBalance - Amount,
            NSTo = ReceiverBalance + Amount,
            Bool = (SenderBalance >= Amount),
            case Bool of
                true ->
                    io:format("Sender has enough funds.~n"),
                    % Calculate new Hash
                    [{_, OHash}|_] = Ledger,
                    Item = io_lib:format("~s~s~w~w~w",[From, To, Amount, NSFrom, NSTo]),
                    NHash = crypto:mac(hmac, sha256, OHash, Item),
                    % Multicast the block
                    [X ! {{From, To, Amount, NSFrom, NSTo}, NHash}|| X <- Group, X =/= self()],
                    node_code([{{From, To, Amount, NSFrom, NSTo}, NHash}|Ledger], Group);
    
                false ->
                    io:format("Sender does not have enough funds.~n"),
                    node_code(Ledger, Group)
            end;
            
        % Node receives the list of peer nodes to be able to multicast later on.
        {List_of_Nodes} ->
            % io:format("Got the list~n"),
            node_code(Ledger, List_of_Nodes)
            

    end.

search_sender_current_balance(_, []) ->
    0;
search_sender_current_balance(Sender, [First_Block|Tail]) ->
    {{User1,_,_,User_Balance1,_},_} = First_Block,
    % io:format("~p~n", [User1]),
    Bool1 = (User1 == Sender),
    case Bool1 of
        true -> User_Balance1;
        false -> 
            {{_,User2,_,_,User_Balance2},_} = First_Block,
            % io:format("~p~n", [User1]),
            Bool2 = (User2 == Sender),
            case Bool2 of
                true -> User_Balance2;
                false -> search_sender_current_balance(Sender, Tail)
            end
    end.

search_receiver_current_balance(_, []) ->
    0;
search_receiver_current_balance(Sender, [First_Block|Tail]) ->
    {{User1,_,_,User_Balance1,_},_} = First_Block,
    Bool1 = (User1 == Sender),
    case Bool1 of
        true -> User_Balance1;
        false -> 
            {{_,User2,_,_,User_Balance2},_} = First_Block,
            Bool2 = (User2 == Sender),
            case Bool2 of
                true -> User_Balance2;
                false -> search_sender_current_balance(Sender, Tail)
            end
    end.