%% @author std-string

%% It is easily proved that no equilateral triangle exists with integral length sides and integral area.
%% However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
%% We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.
%% Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

-module(problem_094).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{1000000000, 518408346}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxPerimeter) ->
    %% ...
    D = 3,
    {X0, Y0} = pell_equation:find_first_solution(D, 1),
    process({X0, Y0}, X0, Y0, D, 1, MaxPerimeter, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

process({X, _Y}, X0, Y0, D, N, MaxPerimeter, Sum) ->
    %% x = 3a +- 2
    %%A1 = (X - 2) div 3,
    %%C1 = 2 * A1 + 1,
    %%P1 = 2 * C1 + 2 * A1,
    P1 = calc_perimeter(X, 1),
    %%A2 = (X + 2) div 3,
    %%C2 = 2 * A1 - 1,
    %%P2 = 2 * C2 + 2 * A2,
    P2 = calc_perimeter(X, -1),
    if
        (P1 > MaxPerimeter) and (P2 > MaxPerimeter) -> Sum;
        (P1 =< MaxPerimeter) and (P2 > MaxPerimeter) -> Sum + P1;
        (P1 > MaxPerimeter) and (P2 =< MaxPerimeter) -> Sum + P2;
        true -> process(pell_equation:find_n_solution({X0, Y0}, D, 1, N + 1), X0, Y0, D, N + 1, MaxPerimeter, Sum + P1 + P2)
    end.

calc_perimeter(X, Sign) ->
    Value = X - 2 * Sign,
    if
        (Value == 0) or (Value rem 3 /= 0) -> 0;
        true ->
            A = Value div 3,
            C = 2 * A + Sign,
            2 * C  + 2 * A
    end.