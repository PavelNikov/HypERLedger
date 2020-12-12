-module(main_d).
-export([init/1, supervise/1]).
-import(ca_d, [ca_init/1]).
-import(helper, [printList/2]).
-import(node_d, [init_node/1]).

init(Node_Host) ->
    % Spawn Nodes and supervisor
    Nodes = createNodeList(Node_Host, [], 15),
    spawn(?MODULE, supervise, [Nodes]),
    % Send list of peers to all Nodes
    [X ! {main, Nodes} || X <- Nodes],
    ok.

% ----------------------------------
% Spawing Nodes and saving their PIDs in a list
% Give them all the same genesis block  
% ----------------------------------
createNodeList(_, Nodes, 0) ->
    Nodes;

createNodeList(Node_Host, Nodes, Num) when Num > 0 ->
    spawn(Node_Host, node_d, init_node, [Num]),
    % Add a new entry to the list the name of the node and a number
    NewList = [{list_to_atom(lists:flatten(io_lib:format("node~p", [Num]))), Node_Host} | Nodes],
    NumNew = Num - 1,
    io:format("Done~n"),
    createNodeList(Node_Host, NewList, NumNew).

% Supervisor that restarts ca in the case it should go down
supervise(Nodes) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(ca_d, ca_init, [Nodes]),
    register(ca, Pid),
    receive
        {'EXIT', Pid, normal} -> 
            unregister(ca),
            ok;
        {'EXIT', Pid, shutdown} -> 
            unregister(ca),
            ok;
        {'EXIT', Pid, _} ->
            supervise(Nodes)
    end.
