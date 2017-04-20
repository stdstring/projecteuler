%% @author std-string

%% If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
%% Find the sum of all the multiples of 3 or 5 below 1000.

-module(problem_001).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{{3, 5, 10 - 1}, 23}, {{3, 5, 1000 - 1}, 233168}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({Number1, Number2, MaxNumber}) ->
    Sum1 = arith_progress(Number1, MaxNumber - MaxNumber rem Number1, MaxNumber div Number1),
    Sum2 = arith_progress(Number2, MaxNumber - MaxNumber rem Number2, MaxNumber div Number2),
    Sum12 = arith_progress(Number1 * Number2, MaxNumber - MaxNumber rem (Number1 * Number2), MaxNumber div (Number1 * Number2)),
    Sum1 + Sum2 - Sum12.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec arith_progress(A1 :: pos_integer(), An :: pos_integer(), N :: pos_integer()) -> pos_integer().
arith_progress(A1, An, N) -> N * (A1 + An) div 2.