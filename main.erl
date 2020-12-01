-module(main).
-export([init/0]).
-import(ca, [ca_code/1]).
-import(helper, [printList/2, automator/1]).

init() ->
    Nodes = createNodeList([], 15),
    timer:sleep(1000),
    register(ca, spawn(ca, ca_init, [Nodes])),
    [X ! {Nodes} || X <- Nodes],
    timer:sleep(500).

% ----------------------------------
% Spawing Nodes and saving their PIDs in a list
% ----------------------------------
createNodeList(Nodes, 0) ->
    printList("Created Nodes list", Nodes),
    Nodes;

createNodeList(Nodes, Num) when Num > 0 ->
    Pid = spawn(node, node_code, [[{{"God", "CA", 576460752303423488, 0, 576460752303423488}, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}], []]),
    NewList = [Pid | Nodes],
    NumNew = Num - 1,
    createNodeList(NewList, NumNew).