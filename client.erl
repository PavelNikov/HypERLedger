-module(client).
-import('lists', [append/2]).
-import('string', [join/2]).
-import('main', []).
-import(crypto,[start/0, hmac/3, mac/4]).
-export([init/0, 
        login/0, 
        choose/0, 
        registerClient/0, 
        sendMoney/1]).

% Cient Application to send Txs to the Central Authority


init() ->
    spawn(?MODULE, choose, []).
    % register(fakeCa, spawn(?MODULE, caStuff, [[]])).

choose() ->
    printLine(),
    io:format("GENERAL INSTRUCTIONS"),
    printLine(),
    io:format("- If there is a choice with numbers, type in the correct number and hit ENTER~n"),
    io:format("- If you have to type in a string of characters, make sure to end with a period~n"),
    timer:sleep(10),

    clr(),
    printLine(),
    io:format("hypERLedger CLIENT APPLICATION"),
    printLine(),
    io:format("1. Create new account~n"),
    io:format("2. Log in~n"),
    io:format("3. Quit~n"),

    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            clr(),
            registerClient();
        2 ->
            clr(),
            login();
        3 ->
            io:format("EXITING~n"),
            clr(),
            exit(self(), ok)
    end.

% Register Client with a new secret name
registerClient() ->
    printLine(),
    io:format("REGISTER CLIENT"),
    printLine(),
    {ok, SecretName} = io:read("Type in a secret name for your new account: "),
    Ca = whereis(ca),
    Ca ! {register, self(), SecretName},
    receive
        {Ca, ok} ->
            io:format("Now you should be able to login with your secret name~n"),
            clr(),
            login();
        {Ca, nope} ->
            io:format("Something went wrong creating your account~n"),
            registerClient()
    end.

login() ->
    printLine(),
    io:format("LOGIN"),
    printLine(),
    io:format("INFO: Make sure no one is looking over your shoulder...~n"),
    {ok, SecretName} = io:read("Type in your secret name to enter your wallet: "),
    Ca = whereis(ca),
    % Try logging in with secret name
    Ca ! {login, self(), SecretName},
    % Wait for answer from CA
    receive 
        {Ca, ok} ->
            wallet(SecretName);
        {Ca, nope} ->
            io:format("WARNING: No match found for ~p. Please make sure it's spelled correctly~n", [SecretName]),
            login()
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
            retrieveBalance(From);
        2 ->
            publicAddress(From);
        3 ->
            clr(),
            sendMoney(From);
        4 ->
            choose();
        5 ->    
           io:format("QUITTING~n"),
           exit(self(), ok) 
    end.

% ----------------------------------
% Retrieving Account Balance
% ----------------------------------

retrieveBalance(From) ->
    clr(),
    printLine(),
    io:format("ACCOUNT BALANCE"),
    printLine(),
    Ca = whereis(ca),
    Ca ! {self(), retrieveBalance, From},
    receive
        {Ca, ok, Balance} ->
            io:format("~p~n", [Balance]);
        {Ca, nope} ->
            io:format("Problem retrieving account balance~n")
    end,
    io:format("1. Back~n"),
    {ok, Choice} = io:read(""),
    case Choice of
        1 ->
            wallet(From)
    end.

% ----------------------------------
% Show public address
% ----------------------------------

publicAddress(SecretName) ->
    printLine(),
    io:format("Public Address"),
    printLine(),

    Ca = whereis(ca),
    Ca ! {self(), retrievePAddr, SecretName},
    
    receive
        {Ca, PublicAddress} ->
            io:format("~s~n", [PublicAddress])
    end,
    io:format("1. Back~n"),
    {ok, Choice} = io:read(""),
    case Choice of
        1 ->
            wallet(SecretName)
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
            wallet(From)
    end.

newTransaction(From) ->
    clr(),
    printLine(),
    io:format("TRANSACTION ZONE"),
    printLine(),
    {ok, To} = io:read("Who do you want to send hyperCoins to?  "),
    {ok, Amount} = io:read("How many hyperCoins do you want to send?  "),
    io:format("~nWARNING: You are about to send ~p hyperCoins to ~w~n", [Amount, To]),
    io:format("Type ok to proceed, no to correct your transaction or qq to stop everything~n"),
    Ca = whereis(ca),
    Answer = io:get_chars("=>", 2),
    if
        Answer == "ok" ->
            ca ! {self(), From, To, Amount};
        Answer == "no" ->
            sendMoney(From);
        Answer == "qq" ->
            io:format("EXITING~n"),
            exit(self(), ok)
    end,
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
    timer:sleep(2000),
    sendMoney(From).


% ----------------------------------
% Helper Functions
% ----------------------------------
printLine() ->
    io:format("~n================================================================~n").

clr() ->
    io:format("\e[H\e[J").