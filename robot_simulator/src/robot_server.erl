-module(robot_server).
-export([robot_server/0, recv/1]).

robot_server() ->
    {ok, ListenSocket} = gen_tcp:listen(8888, [binary, {active, false}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, recv, [AcceptSocket]),
    accept(ListenSocket).

recv(Socket) ->
    {ok, Request} = gen_tcp:recv(Socket, 0),
    Msg = process_request(binary_to_term(Request)),
    ok = gen_tcp:send(Socket, term_to_binary(Msg)).

process_request({init, {Coordinate, Direction}}) ->
    robot_simulator:initialize(Coordinate, Direction);
process_request({control, {RobotState, Commands}}) ->
    robot_simulator:control(RobotState, Commands);
process_request(_) ->
    % Obviously it can't handle all the abnormal case.
    % Just crash for other abnormal cases.
    unknown_command.


	    
	    

    
