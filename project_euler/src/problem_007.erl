%% @author std-string

%% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13. What is the 10001st prime number?

-module(problem_007).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{6, 13}, {10001, 104743}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(DesireCount) -> find_nth_prime(DesireCount).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_nth_prime(DesireCount :: pos_integer()) -> pos_integer().
find_nth_prime(DesireCount) ->
    find_nth_prime(3, 1, DesireCount, [2]).

-spec find_nth_prime(Number :: pos_integer(), Count :: pos_integer(), DesireCount :: pos_integer(), PrimeList :: [pos_integer()]) -> pos_integer().
find_nth_prime(_, DesireCount, DesireCount, PrimeList) -> lists:last(PrimeList);
find_nth_prime(Number, Count, DesireCount, PrimeList) ->
    CheckResult = check_prime(Number, PrimeList),
    if
        CheckResult == true -> find_nth_prime(Number + 2, Count + 1, DesireCount, PrimeList ++ [Number]);
        true -> find_nth_prime(Number + 2, Count, DesireCount, PrimeList)
    end.

-spec check_prime(Number :: pos_integer(), PrimeList :: [pos_integer()]) -> boolean().
check_prime(Number, PrimeList) -> check_prime(Number, trunc(math:sqrt(Number)), PrimeList).

-spec check_prime(Number :: pos_integer(), Bound :: pos_integer(), PrimeList :: [pos_integer()]) -> boolean().
check_prime(Number, Bound, [Prime | PrimeRest]) ->
    if
        Prime > Bound -> true;
        Number rem Prime == 0 -> false;
        Number rem Prime /= 0 -> check_prime(Number, Bound, PrimeRest)
    end.