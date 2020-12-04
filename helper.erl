-module(helper).
-export([printList/2, automator/1, 
        searchList/2, binaryToHex/1, calculatePAddr/1]).
-import(crypto,[start/0, hmac/3, mac/4]).

printList(Message, List) ->
    io:format("~p: ~p~n", [Message, List]).

automator(Recipient) ->
    % Recipient ! {"CA", "pablo", 100},
    % timer:sleep(500),
    % % Recipient ! {"CA", "nina", 50},
    % % timer:sleep(500),
    % % Recipient ! {"CA", "pablo", 30},
    % % timer:sleep(500),
    % % Recipient ! {"CA", "peter", 200},
    % % timer:sleep(500),
    Recipient ! {"CA", "nina", 10},
    timer:sleep(1000),
    Recipient ! {"CA", "paul", 5},
    timer:sleep(1000),
    Recipient ! {"nina", "paul", 5},
    timer:sleep(500).


searchList(_, []) ->
    false;

searchList(Item, List) ->

    [Entry | R] = List,
    Bool = (Item =:= Entry),
    case Bool of
        true ->
            true;
        false ->
            searchList(Item, R)
    end.

calculatePAddr(SecretName) ->
    crypto:start(),
    helper:binaryToHex(crypto:mac(hmac, sha256, atom_to_list(SecretName), "security")).

binaryToHex(Binary) ->
    io_lib:format("a~s",[string:lowercase([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Binary ])]).