-module(robot_simulator).
-include("robot_simulator.hrl").
-export([initialize/2, control/2]).

initialize({X, Y}, Direction) ->
    #robot_state{coordinate = #coordinate{x = X, y = Y}, direction = Direction}.

control(RobotState, {InputFile, OutputFile}) ->
    {ok, Binary} = file:read_file(InputFile),
    List = binary_to_list(Binary),
    ValidCommands = [$l, $r, $f, $b],
    CommandList =
	lists:filter(fun(X) -> lists:member(X, ValidCommands) end, 
		     List),
    {ok, OutFile} = file:open(OutputFile, [write]),
    FinalState = lists:foldl(fun(Cmd, State) ->
				     save_state(State, OutFile),
				     save_command(Cmd, OutFile),
				     execute_command(State, Cmd) end, 
			     RobotState, 
			     CommandList),
    save_state(FinalState, OutFile),
    file:close(OutFile).

save_state(State, OutFile) ->
    DirectionSymbolTab = [{north, $^},
			  {east, $>},
			  {south, $v},
			  {west, $<}],
    DirecSymb =  proplists:get_value(State#robot_state.direction, DirectionSymbolTab),
    Graph = create_graph(State#robot_state.coordinate, DirecSymb),
    file:write(OutFile, list_to_binary(Graph)).
    

create_graph(#coordinate{x=X, y=Y}, DirectionSymbol) ->
    Idx = find_direction_symbol_index(X, Y),
    % One row and column for x = 0 and y = 0
    NColumns = (abs(X) + 1),
    NRows = (abs(Y) + 1),
    Size = NRows * NColumns,
    Elements = erlang:make_tuple(Size,
				 [$[, $ , $]],
				 [{Idx,
				   [$[, DirectionSymbol, $]]}]),
    % Extract element starting from the last one
    add_new_line_per_row(Size, Elements, NColumns, []).

add_new_line_per_row(0, _, _, Lines) ->
    Lines;
add_new_line_per_row(Idx, Elements, NColumns, Lines) when (Idx rem NColumns) == 0 ->
    NewLines = element(Idx, Elements) ++ io_lib:nl() ++ Lines,
    add_new_line_per_row(Idx - 1, Elements, NColumns, NewLines);
add_new_line_per_row(Idx, Elements,  NColumns, Lines) ->
    NewLines = element(Idx, Elements) ++ Lines,
    add_new_line_per_row(Idx - 1, Elements, NColumns, NewLines).


find_direction_symbol_index(X, Y) when X >= 0 andalso Y >= 0 ->
    % Top right, One row and column for x = 0 and y = 0
    abs(X) + 1;
find_direction_symbol_index(X, Y) when X >= 0 andalso Y < 0 ->
    % Bottom righ, One row and column for x = 0 and y = 0 
    (abs(X) + 1) * (abs(Y) + 1);
find_direction_symbol_index(X, Y) when X < 0 andalso Y =< 0 ->
    % Bottom left,One row and column for x = 0 and y = 0
    abs(Y) * (abs(X) + 1) + 1;
find_direction_symbol_index(X, Y) when X < 0 andalso Y > 0 ->
    % Top left, One row and column for x = 0 and y = 0
    1.

save_command(Cmd, OutFile) ->
    Line = "Next Command: " ++ [Cmd] ++ io_lib:nl(),
    file:write(OutFile, list_to_binary(Line)).

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
    
    
