-module(assignment_1).
-export([math/1, palindrome/1]).

-define(REM_OF_ODD_NUM, 1).
-define(REM_OF_EVEN_NUM, 0).

math(List) ->
    case is_list(List) of
	true ->
	    case is_each_element_integer(List) of
		true ->
		    do_math(List);
		false ->
		    {error, "list should contain integers only"}
		end;
	false ->
	    {error, "input must be a list"}
    end.

is_each_element_integer([]) ->
    true;
is_each_element_integer([H | T]) when is_integer(H)->
    is_each_element_integer(T);
is_each_element_integer(_) ->
    false.

do_math(IntegerList) ->
    OddList = [X || X <- IntegerList, X rem 2 == ?REM_OF_ODD_NUM],
    EvenList = [X || X <- IntegerList, X rem 2 == ?REM_OF_EVEN_NUM],
    SumOddNumbers = lists:sum(OddList),
    case EvenList of
	[] ->
	    ProductEvenNumbers = 0;
	_->
	    ProductEvenNumbers = calc_list_product(EvenList, 1)
    end,
    {SumOddNumbers, ProductEvenNumbers}.


calc_list_product([], Product) ->
    Product;
calc_list_product([H | T], Product) ->
    calc_list_product(T, H * Product).

palindrome([]) ->
    [];
palindrome(List) when is_list(List) ->
    case is_each_element_word(List) of
	true ->
	    find_palindrome(List, []);
	false ->
	    {error, "list should contain strings only"}
    end;
palindrome(_) ->
    {error, "input must be a list"}.

% Input is not empty list at the beginning
is_each_element_word([]) ->
    true;
% Empty word is not valid
is_each_element_word([H | T]) when is_list(H), H /= [] ->
    case is_each_element_letter(H) of
	true ->
	    is_each_element_word(T);
	false ->
	    false
    end;
is_each_element_word(_) ->
    false.

% Input is not empty list at the beginning
is_each_element_letter([]) ->
    true;
is_each_element_letter([H | T]) when H >= $a, H =<$z; H >= $A, H =<$Z ->
    is_each_element_letter(T);
is_each_element_letter(_) ->
    false.

find_palindrome([], PalindromeWordList) ->
    PalindromeWordList; 
find_palindrome([H | T], PalindromeWordList) ->
    case is_palindrome(H) of
	true ->
	    NewList = PalindromeWordList ++ [H];
	false ->
	    NewList = PalindromeWordList
    end,
    find_palindrome(T, NewList).

is_palindrome(Word) ->
    ReversedWord = lists:reverse(Word),
    case ReversedWord of
	Word ->
	    true;
	_ ->
	    false
     end.

