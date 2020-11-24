-module(client).
-import('lists', [append/2]).
-import('string', [join/2]).
-export([main/0, 
        login/0, 
        choose/0, 
        registerClient/0, 
        caStuff/1,
        sendMoney/1]).

% Cient Application to send Txs to the Central Authority


main() ->
    spawn(?MODULE, choose, []),
    register(fakeCa, spawn(?MODULE, caStuff, [[]])).

choose() ->
    printLine(),
    io:format("GENERAL INSTRUCTIONS"),
    printLine(),
    io:format("- If there is a choice with numbers, type in the correct number and hit ENTER~n"),
    io:format("- If you have to type in a string of characters, make sure to end with a period~n"),
    timer:sleep(10),

    printLine(),
    io:format("hypERLedger CLIENT APPLICATION"),
    printLine(),
    io:format("1. Create new account~n"),
    io:format("2. Log in~n"),
    io:format("3. Quit~n"),

    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            registerClient();
        2 ->
            login();
        3 ->
            io:format("EXITING~n"),
            exit(self(), ok)
    end.

% Register Client with a new secret name
registerClient() ->
    printLine(),
    io:format("REGISTER CLIENT"),
    printLine(),
    {ok, SecretName} = io:read("Type in a secret name for your new account: "),
    FakeCa = whereis(fakeCa),
    FakeCa ! {register, self(), SecretName},
    receive
        {FakeCa, ok} ->
            timer:sleep(100),
            io:format("Now you should be able to login with your secret name~n"),
            login()
    end.

login() ->
    printLine(),
    io:format("LOGIN"),
    printLine(),
    io:format("INFO: Make sure no one is looking over your shoulder...~n"),
    {ok, SecretName} = io:read("Type in your secret name to enter your wallet: "),
    FakeCa = whereis(fakeCa),
    % Try logging in with secret name
    FakeCa ! {login, self(), SecretName},
    % Wait for answer from CA
    receive 
        {FakeCa, ok} ->
            wallet(SecretName);
        {FakeCa, nope} ->
            io:format("WARNING: No match found for ~p. Please make sure it's spelled correctly~n", [SecretName]),
            login()
    end.

wallet(From) ->
    printLine(),
    io:format("~p's WALLET", [From]),
    printLine(),
    io:format("1. Retrieve Account Balance~n"),
    io:format("2. Send Money~n"),
    io:format("3. Logout~n"),
    io:format("4. Quit~n"),
    
    {ok, Choice} = io:read("=> "),
    case Choice of
        1 ->
            retrieveBalance(From);
        2 ->
            sendMoney(From);
        3 ->
            choose();
        4 ->
           io:format("QUITTING~n"),
           exit(self(), ok) 
    end.

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
    {ok, To} = io:read("Who do you want to send hyperCoins to?  "),
    io:format("~n"),
    {ok, Amount} = io:read("How many hyperCoins do you want to send?  "),
    io:format("~nWARNING: You are about to send ~p hyperCoins to ~w~n", [Amount, To]),
    io:format("Type ok to proceed, no to correct your transaction or qq to stop everything~n"),
    FakeCa = whereis(fakeCa),
    Answer = io:get_chars("=>", 2),
    if
        Answer == "ok" ->
            FakeCa ! {self(), To, From, Amount};
        Answer == "no" ->
            sendMoney(From);
        Answer == "qq" ->
            io:format("EXITING~n"),
            exit(self(), ok)
    end,
    receive
        {FakeCa, ok} ->
            io:format("Transaction complete~n"),
            io:format("Bye Bye Hypercoins"),
            printLine(),
            sendMoney(From)
    end.

retrieveBalance(From) ->
    printLine(),
    io:format("ACCOUNT BALANCE"),
    printLine(),
    io:format("To be implemented~n"),
    io:format("1. Back~n"),
    {ok, Choice} = io:read(" "),
    case Choice of
        1 ->
            wallet(From)
    end.

caStuff(Clients) ->
    receive
        {register, Pid, SecretName} -> 
            io:format("INFO: ca should hash(~p) and save/check hash in list. send ok or nope~n~n", [SecretName]),
            Pid ! {self(), ok},
            caStuff([SecretName | Clients]);

        {login, Pid, SecretName} -> 
            Bool = searchList(SecretName, Clients),
            case Bool of
                true ->
                    Pid ! {self(), ok};
                false ->
                    Pid ! {self(), nope}
            end,
            caStuff(Clients);

        {Client, To, From, Amount} ->
            io:format("INFO: Now would be sending to miner~n"),
            Client ! {self(), ok},
            caStuff(Clients)
    end.

searchList(Item, []) ->
    false;

searchList(Item, List) ->

    [Entry | R] = List,
    Bool = (Item == Entry),
    case Bool of
        true ->
            true;
        false ->
            searchList(Item, R)
    end.

printLine() ->
    io:format("~n================================================================~n").