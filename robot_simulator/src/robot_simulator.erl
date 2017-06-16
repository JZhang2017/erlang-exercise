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

execute_command(Robot, Turn) when Turn == $l orelse Turn == $r ->
    NewDirection = change_direction(Robot#robot_state.direction, Turn),
    Robot#robot_state{direction = NewDirection};
execute_command(Robot, Move) when Move == $f orelse Move == $b ->
    NewCoordinate = move(Robot#robot_state.coordinate, Robot#robot_state.direction, Move),
    Robot#robot_state{coordinate = NewCoordinate};
execute_command(Robot, _) ->
    Robot.

change_direction(Direction, Turn) ->
    DirecTab = [{north, [{$l, west}, {$r, east}]},
		{east, [{$l, north}, {$r, south}]},
		{south, [{$l, east}, {$r, west}]},
		{west, [{$l, south}, {$r, north}]}],
    Directions = proplists:get_value(Direction, DirecTab),
    proplists:get_value(Turn, Directions).

move(Coord, Direct, Move) when Direct == east, Move == $f; Direct == west, Move == $b ->
    NewX = Coord#coordinate.x + 1,
    Coord#coordinate{x = NewX};
move(Coord, Direct, Move) when Direct == east, Move == $b; Direct == west, Move == $f ->
    NewX = Coord#coordinate.x - 1,
    Coord#coordinate{x = NewX};
move(Coord, Direct, Move) when Direct == north, Move == $f; Direct == south, Move == $b ->
    NewY = Coord#coordinate.y + 1,
    Coord#coordinate{y = NewY};
move(Coord, Direct, Move) when Direct == north, Move == $b; Direct == south, Move == $f ->
    NewY = Coord#coordinate.y - 1,
    Coord#coordinate{y = NewY}.
    
    
