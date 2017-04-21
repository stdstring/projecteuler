%% @author std-string

%% n! means n * (n - 1) * ... * 3 * 2 * 1
%% For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800, and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
%% Find the sum of the digits in the number 100!

-module(problem_020).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{100, 648}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(Number) ->
    Result = numbers:factorial(Number),
    Digits = numbers:get_digits(Result),
    lists:sum(Digits).

%% ====================================================================
%% Internal functions
%% ====================================================================