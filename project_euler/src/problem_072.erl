%% @author std-string

%% Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
%% If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
%% 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
%% It can be seen that there are 21 elements in this set.
%% How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?

-module(problem_072).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{8, 21}, {1000000, 303963552391}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    Storage = number_dividers:create_prime_dividers(MaxNumber),
    lists:foldl(fun(Number, Result) ->
        PrimeDividers = sets:to_list(number_dividers:get_number_dividers(Number, Storage)),
        Result + number_dividers:calc_euler_function(Number, PrimeDividers)
    end, 0, lists:seq(2, MaxNumber)).

%% ====================================================================
%% Internal functions
%% ====================================================================