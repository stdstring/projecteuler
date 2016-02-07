%% A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
%% For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
%% A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
%% As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24.
%% By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.
%% However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
%% Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

-module(problem_023).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(INF, 12).
-define(SUP, 28123).

get_check_data() ->
    [{none, 4179871}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) ->
    AbundantNumbers = prepare_search(?INF, []),
    AbundantNumbersSet = sets:from_list(AbundantNumbers),
    Result = process_number(1, AbundantNumbers, AbundantNumbersSet, []),
    lists:sum(Result).

process_number(Number, _AbundantNumbers, _AbundantNumbersSet, Dest) when Number > ?SUP -> Dest;
process_number(Number, AbundantNumbers, AbundantNumbersSet, Dest) ->
    case process_search(Number, AbundantNumbers, AbundantNumbersSet) of
        true -> process_number(Number + 1, AbundantNumbers, AbundantNumbersSet, Dest);
        false -> process_number(Number + 1, AbundantNumbers, AbundantNumbersSet, [Number] ++ Dest)
    end.

process_search(_Number, [], _AbundantNumbersSet) -> false;
process_search(Number, [AbundantNumber | _AbundantNumberRest], _AbundantNumbersSet) when Number < AbundantNumber -> false;
process_search(Number, [AbundantNumber | AbundantNumberRest], AbundantNumbersSet) ->
    Delta = Number - AbundantNumber,
    case sets:is_element(Delta, AbundantNumbersSet) of
        true -> true;
        false -> process_search(Number, AbundantNumberRest, AbundantNumbersSet)
    end.

prepare_search(Number, AbundantNumbers) when Number > ?SUP -> lists:reverse(AbundantNumbers);
prepare_search(Number, AbundantNumbers) ->
    case is_abundant_number(Number) of
        true -> prepare_search(Number + 1, [Number] ++ AbundantNumbers);
        false -> prepare_search(Number + 1, AbundantNumbers)
    end.

is_abundant_number(Number) ->
    Dividers = number_dividers:get_dividers(Number),
    (lists:sum(Dividers) - Number) > Number.