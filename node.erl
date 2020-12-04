-module(node).
-import(crypto,[start/0, hmac/3, mac/4]).
-import(lists, [member/2]).
-import(string, [join/2]).
-export([node_code/2]).

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
        {TxIncluder, From, To, Amount} ->
            SenderBalance = search_user_current_balance(From, Ledger),
            io:format("Sender Balance before tx: ~p~n", [SenderBalance]),
            ReceiverBalance = search_user_current_balance(To, Ledger),
            io:format("Receiver Balance before tx: ~p~n", [ReceiverBalance]),
            NSFrom = SenderBalance - Amount,
            NSTo = ReceiverBalance + Amount,
            Bool = (SenderBalance >= Amount),
            case Bool of
                true ->
                    TxIncluder ! {self(), ok},
                    % Calculate new Hash
                    [{_, OHash}|_] = Ledger,
                    % maybe crypto:start() here?
                    Item = io_lib:format("~s~s~w~w~w",[From, To, Amount, NSFrom, NSTo]),
                    NHash = crypto:mac(hmac, sha256, OHash, Item),
                    % Multicast the block
                    [X ! {{From, To, Amount, NSFrom, NSTo}, NHash}|| X <- Group, X =/= self()],
                    node_code([{{From, To, Amount, NSFrom, NSTo}, NHash}|Ledger], Group);
    
                false ->
                    TxIncluder ! {self(), nope},
                    node_code(Ledger, Group)
            end;
            
        % Node receives the list of peer nodes to be able to multicast later on.
        {List_of_Nodes} ->
            node_code(Ledger, List_of_Nodes);
        
        % retrieve balance and send ok if it is a number
        {Ca, retrieveBalance, PublicAddr} ->
            Balance = search_user_current_balance(PublicAddr, Ledger),
            Ca ! {self(), ok, Balance},
            node_code(Ledger, Group);
        
        % Send Ledger
        {Ca, sendLedger} ->
            Ca ! {self(), ok, Ledger},
            node_code(Ledger, Group)

    end.

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