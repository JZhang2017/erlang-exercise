-module(robot_simulator_distributed_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("robot_simulator.hrl").
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([initialize_robot/1, rotate_robot_left/1, rotate_robot_right/1,
	 move_robot_forward/1, move_robot_backward/1, rotate_and_move_robot/1,
	 handle_invalid_msg/1, rotate_and_move_robot_with_invalid_command/1]).

all() ->
    [{group, init}, {group, control}, {group, abnormality}].

groups() ->
    [{init, [], [initialize_robot]},
     {control, [], [rotate_robot_left,
		    rotate_robot_right,
		    move_robot_forward, 
		    move_robot_backward,
		    rotate_and_move_robot]},
     {abnormality, [], [handle_invalid_msg, 
			rotate_and_move_robot_with_invalid_command]}].

init_per_suite(Config) ->
    {ok, HostName} = inet:gethostname(),
    to_distributed_node(HostName),
    SutHost = "esekiws5109",
    {ok, SutNode} = check_sut_node(SutHost),
    Port = 8888,
    ct:pal("Start server on SUT Node ~p", [SutNode]),
    ServerPid = robot_server:start(SutNode, Port),
    [{sut_host, SutHost} |
     [{port, Port} |
      [{server_pid, ServerPid} | Config]]].

end_per_suite(Config) ->
    ct:pal("Stop server"),
    robot_server:stop(?config(server_pid, Config)),
    net_kernel:stop().

initialize_robot(Config) ->
    Reply = contact_server({init, {{28, -7}, west}}, Config),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 28, y = -7}, direction = west}, Reply).

rotate_robot_left(Config) ->
    State = #robot_state{coordinate = #coordinate{x = -98, y = 35}, direction = south},
    Command = "l",
    Reply = contact_server({control, {State, Command}}, Config),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = -98, y = 35}, direction = east}, Reply).

rotate_robot_right(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 25, y = -35}, direction = west},
    Command = "r",
    Reply = contact_server({control, {State, Command}}, Config),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 25, y = -35}, direction = north}, Reply).

move_robot_forward(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 45, y = 93}, direction = west},
    Command = "f",
    Reply = contact_server({control, {State, Command}}, Config),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 44, y = 93}, direction = west}, Reply).

move_robot_backward(Config) ->
    State = #robot_state{coordinate = #coordinate{x = -136, y = -57}, direction = east},
    Command = "b",
    Reply = contact_server({control, {State, Command}}, Config),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = -137, y = -57}, direction = east}, Reply).

rotate_and_move_robot(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 7, y = 3}, direction = north},
    Commands = "rfflflb",
    Reply = contact_server({control, {State, Commands}}, Config),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 10, y = 4}, direction = west}, Reply).

handle_invalid_msg(Config) ->
    Reply = contact_server("What's up", Config),
    ?assertEqual(unknown_command, Reply).

rotate_and_move_robot_with_invalid_command(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 39, y = -21}, direction = south},
    Commands = "fxl",
    Reply = contact_server({control, {State, Commands}}, Config),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 39, y = -22}, direction = east}, Reply).

contact_server(Request, Config) ->
    ct:pal("Start to connect"),
    {ok, Socket} = gen_tcp:connect(?config(sut_host, Config),
				   ?config(port, Config), 
				   [binary, {active,false}]),
    ok = gen_tcp:send(Socket, term_to_binary(Request)),
    {ok, Reply} = gen_tcp:recv(Socket, 0),
    ct:pal("Close connection"),
    gen_tcp:close(Socket),
    binary_to_term(Reply).

to_distributed_node(HostName) ->
    Node = list_to_atom("testnode@" ++ HostName),
    {ok, _} = net_kernel:start([Node, shortnames]).
			
check_sut_node(HostName) ->
    %% Hardcoded to run system under test on the same machine
    %% in order to avoid firewall issue if it is run another machine
    SutNode = list_to_atom("sutnode@" ++ HostName),
    Result = net_kernel:connect_node(SutNode),
    case Result of
	true ->
	    {ok, SutNode};
	_ ->
	    ct:pal("Error, please set up Node ~p before running test", [SutNode]),
	    {error, SutNode} 
    end.
