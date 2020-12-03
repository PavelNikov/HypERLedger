-module(main).
-export([init/0]).
-import(ca, [ca_init/1]).
-import(helper, [printList/2, automator/1]).

init() ->
    % Unregister some stuff to avoid errors. Only uncomment the line if you enconter the error: ** exception error: bad argument BLAAAAAHHHHH
    % unregister(ca),
    % unregister(txIncluder),

    % Spawn Nodes and register ca
    Nodes = createNodeList([], 15),
    register(ca, spawn(ca, ca_init, [Nodes])),
    [X ! {Nodes} || X <- Nodes],
    timer:sleep(10).

% ----------------------------------
% Spawing Nodes and saving their PIDs in a list
% ----------------------------------
createNodeList(Nodes, 0) ->
    Nodes;

createNodeList(Nodes, Num) when Num > 0 ->
    Pid = spawn(node, node_code, [[{{"0x51DF85A2088B1088AED8CA38A08515F437642CA65BA0BC52637C1CC53DD67EF8", "0x494A64075CBEDAEE8C4DE3D13D5ED2DAC4FAC9A25DD62B7853F953D7473A9326", 576460752303423488, 0, 576460752303423488}, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}], []]),
    NewList = [Pid | Nodes],
    NumNew = Num - 1,
    createNodeList(NewList, NumNew).