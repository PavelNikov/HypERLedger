-module(test).
-export([start/0]).
-import(main, [init/0]).
-import(client, [client_init/0]).


start() ->
    main:init(),
    client:client_init().