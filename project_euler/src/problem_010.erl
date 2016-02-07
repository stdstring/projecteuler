%% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%% Find the sum of all the primes below two million.

-module(problem_010).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{10 - 1, 17}, {2000000 - 1, 142913828922}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) -> lists:sum(eratos_sieve:get_primes(MaxNumber)).