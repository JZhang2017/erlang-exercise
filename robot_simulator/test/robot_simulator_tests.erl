-module(robot_simulator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("robot_simulator.hrl").

-define(INPUT_FILE, "input").

robot_simulator_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [fun rotate_robot_left/0,
      fun rotate_robot_right/0,
      fun move_robot_forward/0,
      fun move_robot_backward/0,
      fun rotate_and_move_robot/0,
      fun rotate_and_move_robot_with_invalid_command/0]}.

setup() ->
    % Not really necessary, just for practicing. 
    % The file is automatically created in each test case when opened.
    {ok, FD} = file:open(?INPUT_FILE, [write]),
    ok = file:close(FD),
    ?INPUT_FILE.

cleanup(InputFile) ->
    ok = file:delete(InputFile).

rotate_robot_left() ->
    RobotInitState = robot_simulator:initialize({-98, 35}, south),
    Command = "l",
    OutputFile = "output_rotate_robot_left",
    Files = prepare_files(Command, OutputFile),
    robot_simulator:control(RobotInitState, Files),
    verify_output_file(OutputFile).

rotate_robot_right() ->
    RobotInitState = robot_simulator:initialize({25, -35}, west),
    Command = "r",
    OutputFile = "output_rotate_robot_right",
    Files = prepare_files(Command, OutputFile),
    robot_simulator:control(RobotInitState, Files),
    verify_output_file(OutputFile).

move_robot_forward() ->
    RobotInitState = robot_simulator:initialize({45, 93}, west),
    Command = "f",
    OutputFile = "output_move_robot_forward",
    Files = prepare_files(Command, OutputFile),
    robot_simulator:control(RobotInitState, Files),
    verify_output_file(OutputFile).

move_robot_backward() ->
    RobotInitState = robot_simulator:initialize({-136, -57}, east),
    Command = "b",
    OutputFile = "output_move_robot_backward",
    Files = prepare_files(Command, OutputFile),
    robot_simulator:control(RobotInitState, Files),
    verify_output_file(OutputFile).

rotate_and_move_robot() ->
    RobotInitState = robot_simulator:initialize({7,3}, north),
    Commands = "rfflflb",
    OutputFile = "output_rotate_and_move_robot",
    Files = prepare_files(Commands, OutputFile),
    robot_simulator:control(RobotInitState, Files),
    verify_output_file(OutputFile).

rotate_and_move_robot_with_invalid_command() ->
    RobotInitState = robot_simulator:initialize({39, -21}, south),
    Commands = "fxl",
    OutputFile = "output_rotate_and_move_robot_with_invalid_command",
    Files = prepare_files(Commands, OutputFile),
    robot_simulator:control(RobotInitState, Files),
    verify_output_file(OutputFile).

prepare_files(Commands, OutputFile) ->
    ok = file:write_file(?INPUT_FILE, Commands),
    {?INPUT_FILE, OutputFile}.

verify_output_file(OutputFile) ->
    {ok, OutputFileContent} = file:read_file(OutputFile),
    ReferenceFile = OutputFile ++ "_ref",
    {ok, ReferenceFileContent} = file:read_file(ReferenceFile),
    ?assertEqual(ReferenceFileContent, OutputFileContent).
