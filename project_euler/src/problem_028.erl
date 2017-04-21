%% @author std-string

%% Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
%%
%% 21 22 23 24 25
%% 20  7  8  9 10
%% 19  6  1  2 11
%% 18  5  4  3 12
%% 17 16 15 14 13
%%
%% It can be verified that the sum of the numbers on the diagonals is 101.
%% What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

-module(problem_028).

-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{5, 101}, {1001, 669171001}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(SideSize) -> solve_impl(SideSize).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% solution:
%% let SideSize - size of helix's side, then N - number of helix (beginning from 0) and SideSize = 2*N+1
%% Perimeter of helix P = 4*(2*N+1) - 4 = 8*N (N > 0)
%% Volume (Sum of perimeters of lower helixes + central number 1) = 1 + Sum(8*k, k = 1..N) = 1 + 4*N*(N+1)
%% For helix with number N corner's number will have follow numbers: V(N-1) + 2*N, V(N-1) + 4*N, V(N-1) + 6*N, V(N-1) + 8*N
%% So, Sum of all diagonal numbers will be follows: 1 + Sum(4*V(k-1) + k*(2 + 4 +6 + 8), k = 1..N)
-spec solve_impl(SideSize :: pos_integer()) -> pos_integer().
solve_impl(SideSize) ->
    N = (SideSize - 1) div 2,
    1 + (4 * N) + (2 * N * (N + 1)) + (8 * N * (N + 1) * (2 * N + 1) div 3).