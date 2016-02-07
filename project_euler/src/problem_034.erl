%% 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
%% Find the sum of all numbers which are equal to the sum of the factorial of their digits.
%% Note: as 1! = 1 and 2! = 2 are not sums they are not included.

-module(problem_034).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{none, 40730}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) ->
    FactorialStorage = prepare_factorials(9),
    %% 7 * 9! == 2540160, number with 7 digits
    traverse_numbers(7 * get_value(9, FactorialStorage), FactorialStorage).

traverse_numbers(Max, FactorialStorage) -> traverse_numbers(3, Max, 0, FactorialStorage).

traverse_numbers(Current, Max, Sum, _FactorialStorage) when Current > Max -> Sum;
traverse_numbers(Current, Max, Sum, FactorialStorage) ->
    DigitsFactorialSum = lists:foldl(fun(Digit, Acc) -> Acc + get_value(Digit, FactorialStorage) end, 0, numbers:get_digits(Current)),
    if
        Current == DigitsFactorialSum -> traverse_numbers(Current + 1, Max, Sum + Current, FactorialStorage);
        Current /= DigitsFactorialSum -> traverse_numbers(Current + 1, Max, Sum, FactorialStorage)
    end.

get_value(Number, FactorialStorage) -> array:get(Number, FactorialStorage).

prepare_factorials(MaxNumber) -> array:from_list([numbers:factorial(Number) || Number <- lists:seq(0, MaxNumber)]).