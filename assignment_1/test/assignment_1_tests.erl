-module(assignment_1_tests).
-include_lib("eunit/include/eunit.hrl").

% math tests
math_odd_even_numbers_test() ->
    Result = assignment_1:math([1,2,3,4]),
    ?assertEqual({4,8}, Result).

math_odd_even_numbers_with_zero_test() ->
    Result = assignment_1:math([1,2,3,4,0]),
    ?assertEqual({4,0}, Result).

math_odd_numbers_only_test() ->
    Result = assignment_1:math([3,7,15,59]),
    ?assertEqual({84,0}, Result).

math_one_odd_number_only_test() ->
    Result = assignment_1:math([79]),
    ?assertEqual({79,0}, Result).

math_even_numbers_only_test() ->
    Result = assignment_1:math([4,8,16,60]),
    ?assertEqual({0,30720}, Result).

math_one_even_number_only_test() ->
    Result = assignment_1:math([78]),
    ?assertEqual({0,78}, Result).

math_empty_list_test() ->
    Result = assignment_1:math([]),
    ?assertEqual({0,0}, Result).

math_non_list_input_test() ->
    Result = assignment_1:math({2}),
    ?assertEqual({error, "input must be a list"}, Result).

math_non_integer_list_input_test() ->
    Result = assignment_1:math([1, {2}, "b", a]),
    ?assertEqual({error, "list should contain integers only"}, Result).

% palindrome tests
mixture_of_palindrome_and_non_palindrome_test() ->
    Result = assignment_1:palindrome(["a", "abba", "neg", "level"]),
    ?assertEqual(["a", "abba", "level"], Result).

palindrome_only_test() ->
    Result = assignment_1:palindrome(["a", "abba", "level"]),
    ?assertEqual(["a", "abba", "level"], Result).

non_palindrome_only_test() ->
    Result = assignment_1:palindrome(["goat", "beach", "great"]),
    ?assertEqual([], Result).

palindrome_non_list_test() ->
    Result = assignment_1:palindrome(a),
    ?assertEqual({error, "input must be a list"}, Result).

palindrome_non_word_list_test() ->
    Result = assignment_1:palindrome([1, abba, {abba}]),
    ?assertEqual({error, "list should contain strings only"}, Result).

palindrome_empty_word_test() ->
    Result = assignment_1:palindrome(["a", "abba", "neg", "", "level"]),
    ?assertEqual({error, "list should contain strings only"}, Result).

palindrome_empty_list_test() ->
    Result = assignment_1:palindrome([]),
    ?assertEqual([], Result).
