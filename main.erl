-module(main).
-export([init/0, supervise/1]).
-import(ca, [ca_init/1]).
-import(helper, [printList/2, automator/1]).
-import('global', [register_name/2, whereis_name/1]).

init() ->
    % Spawn Nodes and supervisor
    Nodes = createNodeList([], 15),
    spawn(?MODULE, supervise, [Nodes]),
    [X ! {Nodes} || X <- Nodes],
    ok.

% ----------------------------------
% Spawing Nodes and saving their PIDs in a list
% ----------------------------------
createNodeList(Nodes, 0) ->
    Nodes;

createNodeList(Nodes, Num) when Num > 0 ->
    Pid = spawn(node, node_code, [[{{"a51DF85A2088B1088AED8CA38A08515F437642CA65BA0BC52637C1CC53DD67EF8", "a494A64075CBEDAEE8C4DE3D13D5ED2DAC4FAC9A25DD62B7853F953D7473A9326", 576460752303423488, 0, 576460752303423488}, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>}], []]),
    NewList = [Pid | Nodes],
    NumNew = Num - 1,
    createNodeList(NewList, NumNew).

% Supervisor that restarts ca in the case it should go down
supervise(Nodes) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(ca, ca_init, [Nodes]),
    global:register_name(ca, Pid),
    receive
        {'EXIT', Pid, normal} -> 
            ok;
        {'EXIT', Pid, shutdown} -> 
            ok;
        {'EXIT', Pid, _} ->
            supervise(Nodes)
    end.