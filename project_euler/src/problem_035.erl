%% The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
%% There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
%% How many circular primes are there below one million?

-module(problem_035).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{100 - 1, 13}, {1000000 - 1, 55}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) ->
    PrimeNumbers = eratos_sieve:get_primes(MaxNumber),
    sets:size(find_all_circular_primes(PrimeNumbers)).

find_all_circular_primes(PrimeNumbers) ->
    find_all_circular_primes(PrimeNumbers, sets:from_list(PrimeNumbers), sets:new()).

find_all_circular_primes([], _PrimeSet, CircularPrimeSet) -> CircularPrimeSet;
find_all_circular_primes([PrimeNumber | PrimeListRest], PrimeSet, CircularPrimeSet) ->
    case sets:is_element(PrimeNumber, CircularPrimeSet) of
        false -> find_all_circular_primes(PrimeListRest, PrimeSet, check_and_add_numbers(create_circular_numbers(PrimeNumber), PrimeSet, CircularPrimeSet));
        true -> find_all_circular_primes(PrimeListRest, PrimeSet, CircularPrimeSet)
    end.

check_and_add_numbers(Numbers, PrimeSet, CircularPrimeSet) ->
    CheckResult = lists:all(fun(Number) -> sets:is_element(Number, PrimeSet) end, Numbers),
    if
        CheckResult == true -> lists:foldl(fun(Number, DestSet) -> sets:add_element(Number, DestSet) end, CircularPrimeSet, Numbers);
        CheckResult == false -> CircularPrimeSet
    end.

create_circular_numbers(PrimeNumber) ->
    lists:map(fun(Digits) -> numbers:get_number(Digits) end, collections:get_all_circular_shift(numbers:get_digits(PrimeNumber))).