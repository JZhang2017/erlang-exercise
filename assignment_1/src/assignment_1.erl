-module(assignment_1).
-export([math/1, palindrome/1]).

-define(REM_OF_ODD_NUM, 1).

math(List) ->
    seq([fun is_input_list/1,
	 fun all_integers/1,
 	 fun do_math/1],
 	List).

seq([], Res) ->
    Res;
seq([Fun | Funs], Data) ->
    case Fun(Data) of
	{error, Response} ->
 	    {error, Response};
 	{ok, NewData} ->
 	    seq(Funs, NewData)
    end.

is_input_list(In) when is_list(In) ->
    {ok, In};
is_input_list(_) ->
    {error, "input must be a list"}.

all_integers(List) ->
    AllIntegers = lists:all(fun(X) -> is_integer(X) end, List),
    case AllIntegers of
	true ->
	    {ok, List};
	false ->
	    {error, "list should contain integers only"}
    end.

do_math(IntegerList) ->
    {OddList, EvenList} = lists:partition(fun(X) -> X rem 2 == ?REM_OF_ODD_NUM end, IntegerList),
    SumOddNumbers = lists:sum(OddList),
    ProductEvenNumbers = calc_list_product(EvenList),
    {ok, {SumOddNumbers, ProductEvenNumbers}}.

calc_list_product([]) ->
    0;
calc_list_product(EvenList) ->
    lists:foldl(fun(X, Acc) -> Acc * X end,
		1,
		EvenList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
palindrome(List) ->
    seq([fun is_input_list/1,
	 fun all_lists/1,
	 fun all_words/1,
 	 fun find_palindrome/1],
 	List).			

all_lists([]) ->
    {ok, []};
all_lists(List) ->
    AllLists = lists:all(fun(X) -> is_list(X) end, List),
    case AllLists of
	true ->
	    {ok, List};
	false ->
	    {error, "list should contain strings only"}
    end.

all_words([]) ->
    {ok, []};
all_words(List) ->
    AllWords = lists:all(fun(X) -> all_letters(X) end, List),
    case AllWords of
	true ->
	    {ok, List};
	false ->
	    {error, "list should contain strings only"}
    end.

all_letters([]) ->
    false;
all_letters(List) ->
    AllLetters = lists:all(fun(X) -> ((X >= $a) and (X =< $z)) or 
                                     ((X >= $A) and (X =<$Z)) end, List).

find_palindrome(Words) ->
    {ok, [ X || X <- Words, is_palindrome(X)]}.

is_palindrome(Word) ->
    Word  == lists:reverse(Word).

