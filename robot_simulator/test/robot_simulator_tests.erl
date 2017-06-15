-module(robot_simulator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("robot_simulator.hrl").

initialize_robot_test() ->
    RobotInitState = robot_simulator:initialize({7,3}, north),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 7, y = 3}, direction = north}, 
       RobotInitState).

rotate_robot_test() ->
    RobotInitState = robot_simulator:initialize({7,3}, north),
    RobotState = robot_simulator:control(RobotInitState, "l"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 7, y = 3}, direction = west}, 
       RobotState).

move_robot_test() ->
    RobotInitState = robot_simulator:initialize({7,3}, north),
    RobotState = robot_simulator:control(RobotInitState, "b"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 7, y = 2}, direction = north}, 
       RobotState).

control_robot_test() ->
    RobotInitState = robot_simulator:initialize({7,3}, north),
    RobotState = robot_simulator:control(RobotInitState, "rfflflb"),
    ?assertEqual(
       #robot_state{coordinate = #coordinate{x = 10, y = 4}, direction = west}, 
       RobotState).
