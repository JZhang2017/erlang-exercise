-module(roman_numerals).
-export([convert/1]).

convert(Integer) when Integer >= 1000 ->
    NumThousands = Integer div 1000,
    Remainder = Integer - NumThousands * 1000,
    lists:duplicate(NumThousands, $M) ++ convert(Remainder);   
convert(Integer) when Integer >= 900 ->
    Remainder = Integer - 900,
    "CM" ++ convert(Remainder);
convert(Integer) when Integer >= 500 ->
    Remainder = Integer - 500,
    "D" ++ convert(Remainder);
convert(Integer) when Integer >= 400 ->
    Remainder = Integer - 400,
    "CD" ++ convert(Remainder);
convert(Integer) when Integer >= 100 ->
    NumHundreds = Integer div 100,
    Remainder = Integer - NumHundreds * 100,
    lists:duplicate(NumHundreds, $C) ++ convert(Remainder);
convert(Integer) when Integer >= 90 ->
    Remainder = Integer - 90,
    "XC" ++ convert(Remainder);
convert(Integer) when Integer >= 50 ->
    Remainder = Integer - 50,
    "L" ++ convert(Remainder);
convert(Integer) when Integer >= 40 ->
    Remainder = Integer - 40,
    "XL" ++ convert(Remainder);
convert(Integer) when Integer >= 10 ->
    NumTens = Integer div 10,
    Remainder = Integer - NumTens * 10,
    lists:duplicate(NumTens, $X) ++ convert(Remainder);
convert(Integer) when Integer >= 9 ->
    Remainder = Integer - 9,
    "IX" ++ convert(Remainder);
convert(Integer) when Integer >= 5 ->
    Remainder = Integer - 5,
    "V" ++ convert(Remainder);
convert(Integer) when Integer >= 4 ->
    Remainder = Integer - 4,
    "IV" ++ convert(Remainder);
convert(Integer) when Integer >= 1 ->
    lists:duplicate(Integer, $I);
convert(0) ->
    [].


-ifdef(debug).
convert(Integer) ->
    %{L, R} = get_low_boundry_representation(Integer),
    R ++ convert(Integer - L).

%get_greatest_low_boundry(Integer) ->
%    [{1,3,"I"},
%     {4,4, "IV"},
%     {5,8, "V"},
%     {9,9, "IX"},
%     {10,39, "X"},
%     {40,49, "XL"}],
    

convert(1) ->
    "I";
convert(Integer) when Integer < 4 ->
    convert(1) ++ convert(Integer - 1);
convert(4) ->
    "IV";
convert(5) ->
    "V";
convert(Integer) when Integer < 9 ->
    convert(5) ++ convert(Integer - 5);
convert(9) ->
    "IX";
convert(10) ->
    "X";
convert(Integer) when Integer < 40 ->
    convert(10) ++ convert(Integer - 10);
convert(40) ->
    "XL";
convert(Integer) ->
    convert(40) ++ convert(Integer - 40).
-endif.

