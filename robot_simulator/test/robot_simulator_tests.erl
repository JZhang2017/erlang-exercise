-module(robot_simulator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("robot_simulator.hrl").

initialize_robot_test() ->
    RobotInitState = robot_simulator:initialize({7, 3}, north),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 7, y = 3}, direction = north}, 
       RobotInitState).

rotate_robot_left_test() ->
    RobotInitState = robot_simulator:initialize({-98, 35}, south),
    RobotState = robot_simulator:control(RobotInitState, "l"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = -98, y = 35}, direction = east}, 
       RobotState).

rotate_robot_right_test() ->
    RobotInitState = robot_simulator:initialize({25, -35}, west),
    RobotState = robot_simulator:control(RobotInitState, "r"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 25, y = -35}, direction = north}, 
       RobotState).

move_robot_backward_test() ->
    RobotInitState = robot_simulator:initialize({-136, -57}, east),
    RobotState = robot_simulator:control(RobotInitState, "b"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = -137, y = -57}, direction = east}, 
       RobotState).

move_robot_forward_test() ->
    RobotInitState = robot_simulator:initialize({45, 93}, west),
    RobotState = robot_simulator:control(RobotInitState, "f"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 44, y = 93}, direction = west}, 
       RobotState).

rotate_and_move_robot_test() ->
    RobotInitState = robot_simulator:initialize({7,3}, north),
    RobotState = robot_simulator:control(RobotInitState, "rfflflb"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 10, y = 4}, direction = west}, 
       RobotState).

invalid_command_test() ->
    RobotInitState = robot_simulator:initialize({39, -21}, south),
    RobotState = robot_simulator:control(RobotInitState, "fxl"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 39, y = -22}, direction = east}, 
       RobotState).
