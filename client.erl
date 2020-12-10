-module(client).
-import('lists', [append/2]).
-import('string', [join/2]).
-import('main', []).
-import(crypto,[start/0, hmac/3, mac/4]).
-import('global', [register_name/2, whereis_name/1]).
-export([init/1, 
        help/1,
        login/1, 
        choose/1, 
        registerClient/1, 
        sendMoney/2,
        printBlockchain/1,
        printList/1]).

% Client Application to send Txs to the Central Authority


init(Ca_Host) ->
    %clr(),
    %timer:sleep(10),
    register(client, spawn(?MODULE, choose, [Ca_Host])).
    %timer:sleep(10).

help(Ca_Host) ->
    printLine(),
    io:format("GENERAL INSTRUCTIONS"),
    printLine(),
    io:format("~p~n", [node()]),
    io:format("- If there is a choice with numbers, type in the correct number and hit ENTER~n"),
    io:format("- If you have to type in a string of characters, make sure to end with a period~n"),
    io:format("- You need to first create an account before being able to login~n"),
    io:format("- Make sure to never loose your Secret Name, as this is the only way to enter your wallet~n"),
    io:format("- To recieve hypercoins retrieve your Public Address from inside your wallet and give that address to the sender~n"),
    io:format("1. Back~n"),
    {ok, Choice} = io:read(""),
    case Choice of
        1 ->
            choose(Ca_Host);
        _ ->
            clr(),
            help(Ca_Host)
    end.

choose(Ca_Host) ->
    printLine(),
    io:format("hypERLedger CLIENT APPLICATION"),
    printLine(),
    io:format("1. Create new account~n"),
    io:format("2. Log in~n"),
    io:format("3. Print Blockchain~n"),
    io:format("4. Help~n"),
    io:format("5. Quit~n"),

    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            clr(),
            registerClient(Ca_Host);
        2 ->
            clr(),
            login(Ca_Host);
        3 ->
            clr(),
            printBlockchain(Ca_Host);
        4 ->
            clr(),
            help(Ca_Host);
        5 ->
            io:format("EXITING~n"),
            clr(),
            exit(self(), ok);
        _ ->
            clr(),
            choose(Ca_Host)
    end.

% Register Client with a new secret name
registerClient(Ca_Host) ->
    printLine(),
    io:format("REGISTER CLIENT"),
    printLine(),
    {ok, SecretName} = io:read("Type in a secret name for your new account: "),
    {ca, Ca_Host}  ! {client, node(), register, SecretName},
    receive
        {ca, ok} ->
            clr(),
            io:format("Success! You may now log in~n"),
            login(Ca_Host);
        {ca, nope, M} ->
            clr(),
            io:format("WARNING: Something went wrong creating your account~n"),
            io:format("WARNING: ~s", [M]),
            choose(Ca_Host)
        after 2000 ->
            timeout
    end.

login(Ca_Host) ->
    printLine(),
    io:format("LOGIN"),
    printLine(),
    io:format("INFO: Make sure no one is looking over your shoulder...~n"),
    {ok, SecretName} = io:read("Type in your secret name to enter your wallet or the number 1 to go back: "),
    case SecretName of 
        1 ->
            clr(),
            choose(Ca_Host);
        _ ->
            % Try logging in with secret name
            {ca, Ca_Host}! {client, node(), login, SecretName},
            % Wait for answer from CA
            receive 
                {ca, ok} ->
                    wallet(SecretName, Ca_Host);
                {ca, nope} ->
                    io:format("WARNING: No match found for ~p. Please make sure it's spelled correctly~n", [SecretName]),
                    login(Ca_Host)
                after 2000 ->
                    timeout
            end
    end.


wallet(From, Ca_Host) ->
    clr(),
    printLine(),
    io:format("~p's WALLET", [From]),
    printLine(),
    io:format("1. Retrieve Account Balance~n"),
    io:format("2. Show Public Address~n"),
    io:format("3. Send Money~n"),
    io:format("4. Logout~n"),
    io:format("5. Quit~n"),
    
    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            clr(),
            retrieveBalance(From, Ca_Host);
        2 ->
            clr(),
            publicAddress(From, Ca_Host);
        3 ->
            clr(),
            sendMoney(From, Ca_Host);
        4 ->
            clr(),
            choose(Ca_Host);
        5 ->    
           io:format("QUITTING~n"),
           exit(self(), ok);
        _ ->
            clr(),
            wallet(From, Ca_Host)
    end.

% ----------------------------------
% Retrieving Account Balance
% ----------------------------------

retrieveBalance(From, Ca_Host) ->
    printLine(),
    io:format("ACCOUNT BALANCE"),
    printLine(),
    {ca, Ca_Host} ! {client, node(), retrieveBalance, From},
    receive
        {ca, ok, Balance} ->
            io:format("~p~n", [Balance]);
        {ca, nope} ->
            io:format("Problem retrieving account balance~n")
        after 2000 ->
            timeout
    end,
    io:format("1. Back~n"),
    {ok, Choice} = io:read(""),
    case Choice of
        1 ->
            wallet(From, Ca_Host);
        _ ->
            clr(),
            retrieveBalance(From, Ca_Host)
    end.

% ----------------------------------
% Show public address
% ----------------------------------

publicAddress(SecretName, Ca_Host) ->
    printLine(),
    io:format("Public Address"),
    printLine(),

    {ca, Ca_Host} ! {client, node(), retrievePAddr, SecretName},
    receive
        {ca, PublicAddress} ->
            io:format("~s~n", [PublicAddress])
        after 2000 ->
            timeout
    end,
    io:format("1. Back~n"),
    {ok, Choice} = io:read(""),
    case Choice of
        1 ->
            wallet(SecretName, Ca_Host);
        _ ->
            clr(),
            publicAddress(SecretName, Ca_Host)
    end.


% ----------------------------------
% Sending Money to another account
% ----------------------------------
sendMoney(From, Ca_Host) ->
    printLine(),
    io:format("TRANSACTION ZONE"),
    printLine(),
    io:format("1. New Transaction~n"),
    io:format("2. Back~n"),
    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            newTransaction(From, Ca_Host);
        2 ->
            wallet(From, Ca_Host);
        _ ->
            clr(),
            sendMoney(From, Ca_Host)
    end.

newTransaction(From, Ca_Host) ->
    clr(),
    printLine(),
    io:format("TRANSACTION ZONE"),
    printLine(),
    {ok, To} = io:read("Who do you want to send hyperCoins to?  "),
    {ok, Amount} = io:read("How many hyperCoins do you want to send?  "),
    io:format("~nWARNING: You are about to send ~p hyperCoins to ~w~n", [Amount, To]),
    io:format("Type ok to proceed, no to correct your transaction or quit to stop everything~n"),
    {ok, Answer} = io:read("=> "),
    case Answer of
        ok ->
            {ca, Ca_Host} ! {client, node(), From, To, Amount};
        no ->
            clr(),
            sendMoney(From, Ca_Host);
        quit ->
            io:format("EXITING~n"),
            clr(),
            exit(self(), ok);
        _ ->
            clr(),
            newTransaction(From, Ca_Host)
    end,
    receive
        {ca, ok, Message} ->
            printLine(),
            io:format("~s~n", [Message]),
            io:format("Transaction complete~n"),
            io:format("Bye Bye Hypercoins"),
            printLine();
        {ca, nope, Message} ->
            printLine(),
            io:format("Transaction failed, please retry~n"),
            io:format("~s", [Message]),
            printLine()
    end,
    sendMoney(From, Ca_Host).

% ----------------------------------
% Print Blockchain
% ----------------------------------

printBlockchain(Ca_Host) ->
    printLine(),
    io:format("Overview of all Transactions"),
    printLine(),

    {ca, Ca_Host} ! {client, node(), printChain},
    receive
        {ca, ok, ChainData} ->
            %[io:format("~p~n", [X]) || X <- ChainData]
            printList(ChainData)
        after 2000 ->
            timeout
    end,
    io:format("1. Back~n"),
    {ok, Answer} = io:read("=> "),
    case Answer of
        1 ->
            choose(Ca_Host);
        _ ->
            clr(),
            printBlockchain(Ca_Host)
    end.


% ----------------------------------
% Helper Functions
% ----------------------------------
printLine() ->
    io:format("~n================================================================~n").

clr() ->
    io:format("\e[H\e[J").

printList([]) ->
    io:format("~n");

printList(List) ->
    [H|T] = List,
    {{From, To, Amount, NSFrom, NSTo}, Hash} = H,
    io:format("___________________________________________________~n~n"),
    io:format("From:~p~nTo: ~p~nAmount: ~w~nNew Balance Sender: ~w~nNew Balance Receiver: ~w~nBlock Hash: ~p~n~n", [From, To, Amount, NSFrom, NSTo, Hash]),
    printList(T).
