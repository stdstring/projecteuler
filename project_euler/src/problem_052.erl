%% It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
%% Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

-module(problem_052).

-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{none, 142857}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) -> search(1).

search(DigitCount) ->
    MinNumber = numbers:power(10, DigitCount - 1),
    MaxNumber = (numbers:power(10, DigitCount) - 1) div 6,
    case search(MinNumber, MaxNumber) of
        {true, SearchedNumber} -> SearchedNumber;
        false -> search(DigitCount - 1)
    end.

search(CurrentNumber, MaxNumber) when MaxNumber < CurrentNumber -> false;
search(CurrentNumber, MaxNumber) ->
    case check_number(CurrentNumber, get_result_numbers(CurrentNumber)) of
        true -> {true, CurrentNumber};
        false -> search(MaxNumber, CurrentNumber + 1)
    end.

check_number(Number, ExpectedResult) ->
    NumberDigits = lists:sort(numbers:get_digits(Number)),
    lists:all(fun(ExpectedNumber) -> NumberDigits == lists:sort(ExpectedNumber) end, ExpectedResult).

get_result_numbers(Number) ->
    [numbers:get_digits(Number * Factor) || Factor <- lists:seq(2, 6)].