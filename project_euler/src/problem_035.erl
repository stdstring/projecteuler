%% @author std-string

%% The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
%% There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
%% How many circular primes are there below one million?

-module(problem_035).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type primes_set() :: sets:set(PrimeNumber :: pos_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{100 - 1, 13}, {1000000 - 1, 55}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    PrimeNumbers = eratos_sieve:get_primes(MaxNumber),
    sets:size(find_all_circular_primes(PrimeNumbers)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_all_circular_primes(PrimeNumbers :: [pos_integer()]) -> primes_set().
find_all_circular_primes(PrimeNumbers) ->
    find_all_circular_primes(PrimeNumbers, sets:from_list(PrimeNumbers), sets:new()).

-spec find_all_circular_primes(PrimeNumbers :: [pos_integer()], PrimeSet :: primes_set(), CircularPrimeSet :: primes_set()) -> primes_set().
find_all_circular_primes([], _PrimeSet, CircularPrimeSet) -> CircularPrimeSet;
find_all_circular_primes([PrimeNumber | PrimeListRest], PrimeSet, CircularPrimeSet) ->
    case sets:is_element(PrimeNumber, CircularPrimeSet) of
        false -> find_all_circular_primes(PrimeListRest, PrimeSet, check_and_add_numbers(create_circular_numbers(PrimeNumber), PrimeSet, CircularPrimeSet));
        true -> find_all_circular_primes(PrimeListRest, PrimeSet, CircularPrimeSet)
    end.

-spec check_and_add_numbers(Numbers :: [pos_integer()], PrimeSet :: primes_set(), CircularPrimeSet :: primes_set()) -> primes_set().
check_and_add_numbers(Numbers, PrimeSet, CircularPrimeSet) ->
    case lists:all(fun(Number) -> sets:is_element(Number, PrimeSet) end, Numbers) of
        true -> lists:foldl(fun(Number, DestSet) -> sets:add_element(Number, DestSet) end, CircularPrimeSet, Numbers);
        false -> CircularPrimeSet
    end.

-spec create_circular_numbers(PrimeNumber :: pos_integer()) -> [pos_integer()].
create_circular_numbers(PrimeNumber) ->
    lists:map(fun(Digits) -> numbers:get_number(Digits) end, collections:get_all_circular_shift(numbers:get_digits(PrimeNumber))).