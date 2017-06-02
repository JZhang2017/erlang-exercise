-module(roman_numerals_tests).
-include_lib("eunit/include/eunit.hrl").
-ifdef(debug).
represent_number_one_in_roman_numeral_system_test() ->
    Res = roman_numerals:convert(1),
    ?assertEqual("I", Res).

represent_number_three_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3),
    ?assertEqual("III", Res).

represent_number_four_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(4),
    ?assertEqual("IV", Res).

represent_number_five_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(5),
    ?assertEqual("V", Res).

represent_number_six_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(6),
    ?assertEqual("VI", Res).

represent_number_nine_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(9),
    ?assertEqual("IX", Res).

 represent_number_ten_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(10),
    ?assertEqual("X", Res).

represent_number_thirteen_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(13),
    ?assertEqual("XIII", Res).

represent_number_fourty_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(40),
    ?assertEqual("XL", Res).

represent_number_fourty_six__in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(46),
    ?assertEqual("XLVI", Res).

represent_number_fourty_six__in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(46),
    ?assertEqual("XLVI", Res).

-endif.
represent_number_three_thousand_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3000),
    ?assertEqual("MMM", Res).

represent_number_three_thousand_nine_hundred_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3900),
    ?assertEqual("MMMCM", Res).
represent_number_three_thousand_eight_hundred_twenty_seven_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3827),
    ?assertEqual("MMMDCCCXXVII", Res).
represent_number_three_thousand_five_hundred_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3500),
    ?assertEqual("MMMD", Res).
represent_number_three_thousand_four_hundred_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3400),
    ?assertEqual("MMMCD", Res).
represent_number_three_thousand_three_hundred_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3300),
    ?assertEqual("MMMCCC", Res).
represent_number_three_thousand_one_hundred_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3100),
    ?assertEqual("MMMC", Res).
represent_number_three_thousand_one_hundred_ninety_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3190),
    ?assertEqual("MMMCXC", Res).
represent_number_three_thousand_four_hundred_ninety_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3490),
    ?assertEqual("MMMCDXC", Res).
represent_number_three_thousand_one_hundred_fifty_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3150),
    ?assertEqual("MMMCL", Res).
represent_number_three_thousand_one_hundred_forty_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3140),
    ?assertEqual("MMMCXL", Res).
represent_number_three_thousand_one_hundred_thirty_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3130),
    ?assertEqual("MMMCXXX", Res).
represent_number_three_thousand_one_hundred_sixty_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3160),
    ?assertEqual("MMMCLX", Res).
represent_number_three_thousand_one_hundred_nine_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3109),
    ?assertEqual("MMMCIX", Res).
represent_number_three_thousand_one_hundred_five_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3105),
    ?assertEqual("MMMCV", Res).
represent_number_three_thousand_one_hundred_four_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3104),
    ?assertEqual("MMMCIV", Res).
represent_number_three_thousand_one_hundred_one_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3101),
    ?assertEqual("MMMCI", Res).
represent_number_three_thousand_one_hundred_eight_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3108),
    ?assertEqual("MMMCVIII", Res).
represent_number_one_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(1),
    ?assertEqual("I", Res).
represent_number_three_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(3),
    ?assertEqual("III", Res).
represent_number_four_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(4),
    ?assertEqual("IV", Res).
represent_number_five_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(5),
    ?assertEqual("V", Res).
represent_number_six_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(6),
    ?assertEqual("VI", Res).
represent_number_nine_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(9),
    ?assertEqual("IX", Res).
represent_number_ten_in_roman_numberal_system_test() ->
    Res = roman_numerals:convert(10),
    ?assertEqual("X", Res).


