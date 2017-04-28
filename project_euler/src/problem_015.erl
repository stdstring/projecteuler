%% @author std-string

%% Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
%% How many such routes are there through a 20×20 grid?

-module(problem_015).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{2, 6}, {3, 20}, {20, 137846528820}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(Size) ->
    %% Solution for N * N grid:
    %% 1) each path contains N horizontal and N vertical regions
    %% 2) It is enough N horizontal regions (or N horizontal regions) for path definition
    %% So for solution we may calculate number of combinations by N from 2 * N (calculate binomial coefficient)
    numbers:calc_binomial_coeff(2 * Size, Size).

%% ====================================================================
%% Internal functions
%% ====================================================================