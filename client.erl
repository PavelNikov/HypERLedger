-module(client).
-import('lists', [append/2]).
-import('string', [join/2]).
-import('main', []).
-import(crypto,[start/0, hmac/3, mac/4]).
-import('global', [register_name/2, whereis_name/1]).
-export([init/0, 
        login/0, 
        choose/0, 
        registerClient/0, 
        sendMoney/1,
        printBlockchain/0,
        printList/1]).

% Cient Application to send Txs to the Central Authority


init() ->
    clr(),
    printLine(),
    io:format("GENERAL INSTRUCTIONS"),
    printLine(),
    io:format("- If there is a choice with numbers, type in the correct number and hit ENTER~n"),
    io:format("- If you have to type in a string of characters, make sure to end with a period~n"),
    spawn(?MODULE, choose, []).
    
choose() ->
    printLine(),
    io:format("hypERLedger CLIENT APPLICATION"),
    printLine(),
    io:format("1. Create new account~n"),
    io:format("2. Log in~n"),
    io:format("3. Print Blockchain~n"),
    io:format("4. Quit~n"),

    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            clr(),
            registerClient();
        2 ->
            clr(),
            login();
        3 ->
            clr(),
            printBlockchain();
        4 ->
            io:format("EXITING~n"),
            clr(),
            exit(self(), ok);
        _ ->
            clr(),
            choose()
    end.

% Register Client with a new secret name
registerClient() ->
    printLine(),
    io:format("REGISTER CLIENT"),
    printLine(),
    {ok, SecretName} = io:read("Type in a secret name for your new account: "),
    ca ! {register, self(), SecretName},
    Ca = whereis(ca),
    receive
        {Ca, ok} ->
            %io:format("Now you should be able to login with your secret name~n"),
            clr(),
            io:format("Success! You may now log in~n"),
            login();
        {Ca, nope, M} ->
            clr(),
            io:format("WARNING: Something went wrong creating your account~n"),
            io:format("WARNING: ~s", [M]),
            choose()
        after 2000 ->
            timeout
    end.

login() ->
    printLine(),
    io:format("LOGIN"),
    printLine(),
    io:format("INFO: Make sure no one is looking over your shoulder...~n"),
    {ok, SecretName} = io:read("Type in your secret name to enter your wallet or the number 1 to go back: "),
    case SecretName of 
        1 ->
            clr(),
            choose();
        _ ->
            % Try logging in with secret name
            ca ! {login, self(), SecretName},
            % Wait for answer from CA
            Ca = whereis(ca),
            receive 
                {Ca, ok} ->
                    wallet(SecretName);
                {Ca, nope} ->
                    io:format("WARNING: No match found for ~p. Please make sure it's spelled correctly~n", [SecretName]),
                    login()
                after 2000 ->
                    timeout
            end
    end.


wallet(From) ->
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
            retrieveBalance(From);
        2 ->
            clr(),
            publicAddress(From);
        3 ->
            clr(),
            sendMoney(From);
        4 ->
            clr(),
            choose();
        5 ->    
           io:format("QUITTING~n"),
           exit(self(), ok);
        _ ->
            clr(),
            wallet(From)
    end.

% ----------------------------------
% Retrieving Account Balance
% ----------------------------------

retrieveBalance(From) ->
    printLine(),
    io:format("ACCOUNT BALANCE"),
    printLine(),
    ca ! {self(), retrieveBalance, From},
    Ca = whereis(ca),
    receive
        {Ca, ok, Balance} ->
            io:format("~p~n", [Balance]);
        {Ca, nope} ->
            io:format("Problem retrieving account balance~n")
        after 2000 ->
            timeout
    end,
    io:format("1. Back~n"),
    {ok, Choice} = io:read(""),
    case Choice of
        1 ->
            wallet(From);
        _ ->
            clr(),
            retrieveBalance(From)
    end.

% ----------------------------------
% Show public address
% ----------------------------------

publicAddress(SecretName) ->
    printLine(),
    io:format("Public Address"),
    printLine(),

    ca ! {self(), retrievePAddr, SecretName},
    Ca = whereis(ca),    
    receive
        {Ca, PublicAddress} ->
            io:format("~s~n", [PublicAddress])
        after 2000 ->
            timeout
    end,
    io:format("1. Back~n"),
    {ok, Choice} = io:read(""),
    case Choice of
        1 ->
            wallet(SecretName);
        _ ->
            clr(),
            publicAddress(SecretName)
    end.


% ----------------------------------
% Sending Money to another account
% ----------------------------------
sendMoney(From) ->
    printLine(),
    io:format("TRANSACTION ZONE"),
    printLine(),
    io:format("1. New Transaction~n"),
    io:format("2. Back~n"),
    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            newTransaction(From);
        2 ->
            wallet(From);
        _ ->
            clr(),
            sendMoney(From)
    end.

newTransaction(From) ->
    clr(),
    printLine(),
    io:format("TRANSACTION ZONE"),
    printLine(),
    % {ok, [To]} = io:fread("Who do you want to send hyperCoins to? ", "~w"),
    % {ok, Amount} = io:read("How many hyperCoins do you want to send?  "),
    {ok, To} = io:read("Who do you want to send hyperCoins to?  "),
    {ok, Amount} = io:read("How many hyperCoins do you want to send?  "),
    io:format("~nWARNING: You are about to send ~p hyperCoins to ~w~n", [Amount, To]),
    io:format("Type ok to proceed, no to correct your transaction or quit to stop everything~n"),
    {ok, Answer} = io:read("=> "),
    case Answer of
        ok ->
            ca ! {self(), From, To, Amount};
        no ->
            clr(),
            sendMoney(From);
        quit ->
            io:format("EXITING~n"),
            clr(),
            exit(self(), ok);
        _ ->
            clr(),
            newTransaction(From)
    end,
    Ca = whereis(ca),
    receive
        {Ca, ok, Message} ->
            printLine(),
            io:format("~s~n", [Message]),
            io:format("Transaction complete~n"),
            io:format("Bye Bye Hypercoins"),
            printLine();
        {Ca, nope, Message} ->
            printLine(),
            io:format("Transaction failed, please retry~n"),
            io:format("~s", [Message]),
            printLine()
    end,
    sendMoney(From).

% ----------------------------------
% Print Blockchain
% ----------------------------------

printBlockchain() ->
    printLine(),
    io:format("Overview of all Transactions"),
    printLine(),

    ca ! {self(), printChain},
    Ca = whereis(ca),
    receive
        {Ca, ok, ChainData} ->
            %[io:format("~p~n", [X]) || X <- ChainData]
            printList(ChainData)
        after 2000 ->
            timeout
    end,
    io:format("1. Back~n"),
    {ok, Answer} = io:read("=> "),
    case Answer of
        1 ->
            choose();
        _ ->
            clr(),
            printBlockchain()
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
