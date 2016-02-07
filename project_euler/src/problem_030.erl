%% Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
%% 1634 = 1^4 + 6^4 + 3^4 + 4^4
%% 8208 = 8^4 + 2^4 + 0^4 + 8^4
%% 9474 = 9^4 + 4^4 + 7^4 + 4^4
%% As 1 = 1^4 is not a sum it is not included.
%% The sum of these numbers is 1634 + 8208 + 9474 = 19316.
%% Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

-module(problem_030).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{4, 19316}, {5, 443839}].

prepare_data(_ModuleSourceDir, Power) ->
    Delta = numbers:power(9, Power),
    Infimum = find_infimum(Delta),
    {Power, Infimum}.

solve({Power, Infimum}) ->
    PowerStorage = prepare_powers(9, Power),
    traverse_numbers(Infimum, PowerStorage).

find_infimum(Delta) ->
    find_infimum(1, 10, Delta, Delta).

find_infimum(LeftBound, RightBound, Value, _Delta) when (LeftBound < Value) and (Value < RightBound) -> Value;
find_infimum(LeftBound, RightBound, Value, Delta) ->
    find_infimum(10 * LeftBound, 10 * RightBound, Value + Delta, Delta).

traverse_numbers(Max, PowerStorage) -> traverse_numbers(2, Max, 0, PowerStorage).

traverse_numbers(Current, Max, Sum, _) when Current > Max -> Sum;
traverse_numbers(Current, Max, Sum, PowerStorage) ->
    DigitsPowerSum = lists:foldl(fun(Digit, Acc) -> Acc + get_value(Digit, PowerStorage) end, 0, numbers:get_digits(Current)),
    if
        Current == DigitsPowerSum -> traverse_numbers(Current+1, Max, Sum + Current, PowerStorage);
        Current /= DigitsPowerSum -> traverse_numbers(Current+1, Max, Sum, PowerStorage)
    end.

get_value(Number, PowerStorage) -> array:get(Number, PowerStorage).

prepare_powers(MaxNumber, Power) -> array:from_list([numbers:power(Number, Power) || Number <- lists:seq(0, MaxNumber)]).