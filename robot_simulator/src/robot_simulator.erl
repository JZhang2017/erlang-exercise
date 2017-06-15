-module(robot_simulator).
-include("robot_simulator.hrl").
-export([initialize/2, control/2]).

initialize({X, Y}, Direction) ->
    #robot_state{coordinate = #coordinate{x = X, y = Y}, direction = Direction}.

control(RobotState, []) ->
    RobotState;
control(RobotState, [H | T]) ->
    NewRobotState = execute_command(RobotState, H),
    control(NewRobotState, T).

execute_command(#robot_state{coordinate = Coordinate, direction = Direction}, Turn) when Turn == $l; Turn == $r ->
    NewDirection = change_direction(Direction, Turn),
    #robot_state{coordinate = Coordinate, direction = NewDirection};
execute_command(#robot_state{coordinate = Coordinate, direction = Direction}, Move) when Move == $f; Move == $b ->
    NewCoordinate = move(Coordinate, Direction, Move),
    #robot_state{coordinate = NewCoordinate, direction = Direction};
execute_command(RobotState, _) ->
    RobotState.

change_direction(Direction, Turn) ->
    %           current, left, right
    DirecTab = [{north, west, east},
		{east, north, south},
	        {south, east, west},
	        {west, south, north}],
    {_, LeftDirection, RightDirection} = lists:keyfind(Direction, 1, DirecTab),
    case Turn == $l of
	true ->
	    LeftDirection;
	false ->
	    RightDirection
    end.

move(#coordinate{x = X, y = Y}, Direct, Move) when Direct == east, Move == $f; Direct == west, Move == $b ->
    #coordinate{x = X + 1, y = Y};
move(#coordinate{x = X, y = Y}, Direct, Move) when Direct == east, Move == $b; Direct == west, Move == $f ->
    #coordinate{x = X - 1, y = Y};
move(#coordinate{x = X, y = Y}, Direct, Move) when Direct == north, Move == $f; Direct == south, Move == $b ->
    #coordinate{x = X, y = Y + 1};
move(#coordinate{x = X, y = Y}, Direct, Move) when Direct == north, Move == $b; Direct == south, Move == $f ->
    #coordinate{x = X, y = Y - 1}.
    
    
