-module(robot_simulator_socket_SUITE).
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
    ct:pal("Start server"),
    ServerPid = robot_server:start(8888),
    ConfigWithSeverPid = [{server_pid, ServerPid} | Config],
    [{port, 8888} | ConfigWithSeverPid].

end_per_suite(Config) ->
     ct:pal("Stop server"),
    ServerPid = ?config(server_pid, Config),
    robot_server:stop(ServerPid).

initialize_robot(Config) ->
    Reply = contact_server({init, {{28, -7}, west}}, ?config(port, Config)),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 28, y = -7}, direction = west}, Reply).

rotate_robot_left(Config) ->
    State = #robot_state{coordinate = #coordinate{x = -98, y = 35}, direction = south},
    Command = "l",
    Reply = contact_server({control, {State, Command}}, ?config(port, Config)),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = -98, y = 35}, direction = east}, Reply).

rotate_robot_right(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 25, y = -35}, direction = west},
    Command = "r",
    Reply = contact_server({control, {State, Command}}, ?config(port, Config)),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 25, y = -35}, direction = north}, Reply).

move_robot_forward(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 45, y = 93}, direction = west},
    Command = "f",
    Reply = contact_server({control, {State, Command}}, ?config(port, Config)),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 44, y = 93}, direction = west}, Reply).

move_robot_backward(Config) ->
    State = #robot_state{coordinate = #coordinate{x = -136, y = -57}, direction = east},
    Command = "b",
    Reply = contact_server({control, {State, Command}}, ?config(port, Config)),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = -137, y = -57}, direction = east}, Reply).

rotate_and_move_robot(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 7, y = 3}, direction = north},
    Commands = "rfflflb",
    Reply = contact_server({control, {State, Commands}}, ?config(port, Config)),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 10, y = 4}, direction = west}, Reply).

handle_invalid_msg(Config) ->
    Reply = contact_server("What's up", ?config(port, Config)),
    ?assertEqual(unknown_command, Reply).

rotate_and_move_robot_with_invalid_command(Config) ->
    State = #robot_state{coordinate = #coordinate{x = 39, y = -21}, direction = south},
    Commands = "fxl",
    Reply = contact_server({control, {State, Commands}}, ?config(port, Config)),
    ?assertEqual(#robot_state{coordinate = #coordinate{x = 39, y = -22}, direction = east}, Reply).

contact_server(Request, Port) ->
    ct:pal("Start to connect"),
    {ok, Socket} = gen_tcp:connect({127,0,0,1},
				   Port, 
				   [binary, {active,false}]),
    ok = gen_tcp:send(Socket, term_to_binary(Request)),
    {ok, Reply} = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
    binary_to_term(Reply).
			

