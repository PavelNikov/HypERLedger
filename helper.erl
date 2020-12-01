-module(helper).
-export([printList/2, automator/1, 
        searchList/2, binaryToHex/1]).

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

binaryToHex(Binary) ->
    io:format("<<~s>>~n", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Binary ]]).