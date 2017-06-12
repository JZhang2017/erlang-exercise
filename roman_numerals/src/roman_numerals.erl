-module(roman_numerals).
-export([convert/1]).

-record(boundry, {value,
		  roman_num,
		  is_duplicable = false}).

convert(Integer) ->
    Boundries = 
	[#boundry{value = 1000, roman_num = "M", is_duplicable = true},
	 #boundry{value = 900, roman_num = "CM"},
	 #boundry{value = 500, roman_num = "D"},
	 #boundry{value = 400, roman_num = "CD"},
	 #boundry{value = 100, roman_num = "C",is_duplicable = true},
	 #boundry{value = 90, roman_num = "XC"},
	 #boundry{value = 50, roman_num = "L"},
	 #boundry{value = 40, roman_num = "XL"},
	 #boundry{value = 10, roman_num = "X",is_duplicable = true},
	 #boundry{value = 9, roman_num = "IX"},
	 #boundry{value = 5, roman_num = "V"},
	 #boundry{value = 4, roman_num = "IV"},
	 #boundry{value = 1, roman_num = "I",is_duplicable = true}],
    convert(Integer, Boundries, []).

convert(0, _, RomanNumRes) ->
    RomanNumRes;
convert(Integer, [#boundry{value = Value, roman_num = RomanNum, is_duplicable = Duplicable} | Tail], RomanNumRes) when Integer >= Value ->
    case Duplicable of 
	true ->
	    Multiple = Integer div Value,
	    NewInteger = Integer - Multiple * Value,
	    [Elem] = RomanNum,
	    NewRomanNumRes = RomanNumRes ++ lists:duplicate(Multiple, Elem);	
	false ->
	    NewInteger = Integer - Value,
	    NewRomanNumRes = RomanNumRes ++ RomanNum
    end,
    convert(NewInteger, Tail, NewRomanNumRes);	    
convert(Integer, [ _ | Tail], RomanNumRes) ->
    convert(Integer, Tail, RomanNumRes).









-ifdef(debug).
convert(Integer) ->
    %{L, R} = get_low_boundry_representation(Integer),
    R ++ convert(Integer - L).


    

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

