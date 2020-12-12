-module(helper).
-export([printList/2,
        searchList/2, binaryToHex/1, calculatePAddr/1]).
-import(crypto,[start/0, hmac/3, mac/4]).
-import(lists, [flatten/2]).

% Print any given list with a Message before each element
printList(Message, List) ->
    io:format("~p: ~p~n", [Message, List]).


% Recursively Search a list
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

% Hasing of secret name and transformation to binary
calculatePAddr(SecretName) ->
    crypto:start(),
    helper:binaryToHex(crypto:mac(hmac, sha256, atom_to_list(SecretName), "security")).

binaryToHex(Binary) ->
    % Prepend an a to each hex value to show that it's an address
    "a" ++ string:lowercase([io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Binary ]).
