-module(robot_server).
-export([start/1, stop/1, supervisor/1, listener/1, transaction_handler/1]).

start(Port) ->
    spawn(?MODULE, supervisor, [Port]).

stop(SupervisorPid) ->
    SupervisorPid ! stop.

supervisor(Port) ->
    spawn_link(?MODULE, listener, [Port]),
    receive
	stop ->
	    exit(ordered_exit)
    end.

listener(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    Result = gen_tcp:accept(ListenSocket),
    case Result of
	{ok, AcceptSocket} ->
	    Handler = spawn_link(?MODULE, transaction_handler, [AcceptSocket]),
	    gen_tcp:controlling_process(AcceptSocket, Handler),
	    accept(ListenSocket);
	{error, closed} ->
	    ok;
	 _ ->
	   accept(ListenSocket)
    end.

transaction_handler(Socket) ->
    receive
	{tcp_closed, _} ->
	    gen_tcp:close(Socket);
	{tcp_error, _, _} ->
	    gen_tcp:close(Socket);
	{tcp, _, Request} ->
	   Msg = process_request(binary_to_term(Request)),
	   ok = gen_tcp:send(Socket, term_to_binary(Msg)),
	    transaction_handler(Socket);
	_ ->
	   transaction_handler(Socket)
    end.

process_request({init, {Coordinate, Direction}}) ->
    robot_simulator:initialize(Coordinate, Direction);
process_request({control, {RobotState, Commands}}) ->
    robot_simulator:control(RobotState, Commands);
process_request(_) ->
    % Obviously it can't handle all the abnormal case.
    % Just crash for other abnormal cases.
    unknown_command.
