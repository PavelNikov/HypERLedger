-module(node).
-import(crypto,[start/0, hmac/3]).
-import(lists, [member/2]).
-import(string, [join/2]).
-export([main/0,node_code/1]).

main() ->
    Pid = spawn(?MODULE, node_code, [[{{"None", "None", "None", "None", "None"}, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}]]),
    Pid ! {{"Me", "You", 5, 200, 305}, <<27,56,253,239,38,191,119,10,129,86,191,11,40,195,62,243,243,136,238,110>>},
    Pid ! {{"He", "Her", 5, 195, 310}, <<37,51,111,20,179,159,162,246,192,112,66,51,68,61,54,112,2,202,47,222>>},
    Pid ! {{"Me", "You", 5}},
    ok.
    % Pid ! {{"You", "Me", 5, 200, 305}, 8797766}.

node_code(Ledger) ->
    % io:format("~p~n", [Ledger]),

    receive
        % Receive a new Block:
        {{From, To, Amount, NSFrom, NSTo}, NHash} -> 
            % Retrieve the old hash value:
            [{_, OHash}|R] = Ledger,

            % Compute the hash value by taking the OHash has the key and the Info as the data:
            crypto:start(),
            % Works with: "{From, To, Amount, NSFrom, NSTo}"
            % item = string:join([From, To, Amount, NSFrom, NSTo], ", "),
            % io:format(item),
            Hash = crypto:hmac(sha, OHash, "{From, To, Amount, NSFrom, NSTo}"),
            % io:format("~p~n", [Hash]),
            
            % Check whether NHash and Hash match. If they do, update ledger. Otherwise refuse to update:
            Bool = (Hash == NHash),
            case Bool of
                true -> 
                    io:format("The hash values match. Ledger is updated.~n"),
                    node_code([{{From, To, Amount, NSFrom, NSTo}, NHash}|Ledger]);
                false -> 
                    io:format("The hash values don't match. Transaction is rejected.~n"),
                    node_code(Ledger)
            end;

        {{From, To, Amount}} ->
            SenderBalance = search_sender_current_balance(From, Ledger),
            ReceiverBalance = search_receiver_current_balance(To, Ledger),
            Bool = (SenderBalance >= Amount),
            case Bool of
                true ->
                    io:format("Sender has enough funds.~n");
                % %%%%%%%%%%%%%%%%%%%%%%%%%% CONTINUE HERE %%%%%%%%%%%%%%%%%%%%%%%%%%%%% (i) Calculate hash (ii) Multicast Transaction.
                    
                false ->
                    io:format("Sender does not have enough funds.~n")
            end
    end.

search_sender_current_balance(_, []) ->
    0;
search_sender_current_balance(Sender, [First_Block|Tail]) ->
    {{User,_,_,User_Balance,_},_} = First_Block,
    Bool = (User == Sender),
    case Bool of
        true -> User_Balance;
        false -> search_sender_current_balance(Sender, Tail)
    end.

search_receiver_current_balance(_, []) ->
    0;
search_receiver_current_balance(Receiver, [First_Block|Tail]) ->
    {{_,User,_,_,User_Balance},_} = First_Block,
    io:format("~p~n", [Tail]),
    Bool = (User == Receiver),
    case Bool of
        true -> User_Balance;
        false -> search_receiver_current_balance(Receiver, Tail)
    end.