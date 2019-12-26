%% @author std-string

%% It is easily proved that no equilateral triangle exists with integral length sides and integral area.
%% However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
%% We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.
%% Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

-module(problem_094).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(D, 3).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{1000000000, 518408346}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxPerimeter) ->
    %% we have the triangle with sides C, C, C+-1
    %% S^2 = (p/2)*((p/2) - C)*((p/2) - C)*((p/2) - (C+-1)) = p*(p - 2*C)*(p - 2*C)*(p - 2*(C+-1))/16 where p = C + C + (C+-1) - perimeter
    %% if C - even => C+-1 - odd => p - odd, 2*(C+-1) - even => p - 2*(C+-1) - odd, p - 2*C -odd =>
    %% p*(p - 2*C)*(p - 2*C)*(p - 2*(C+-1)) - odd => p*(p - 2*C)*(p - 2*C)*(p - 2*(C+-1)) isn't divided by 16 => C - odd, C+-1 - even
    %% Let B - height (and median) on side C+-1 => 2*A = C+-1 and triangle with sides A, B, C is right triangle
    %% 2*A = C+-1 or C = 2*A+-1 - equivalent definitions
    %% A^2 + B^2 = C^2 = (2*A+-1)^2 = 4*A^2 +- 4*A + 1 =>
    %% B^2 = 4*A^2 +- 4*A + 1 - A^2 = 3*A^2 +- 4*A + 1 => 3*B^2 = 9*A^2 +- 12*A + 3 = (9*A^2 +- 12*A + 4) - 1 = (3*A+-2)^2 - 1
    %% so, 3*B^2 = (3*A+-2)^2 - 1 => (3*A+-2)^2 - 3*B^2 = 1 - Pell equation
    FirstSolution = pell_equation:find_first_solution(?D, 1),
    process(FirstSolution, FirstSolution, 1, MaxPerimeter, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(Solution :: pell_equation:solution(),
              FirstSolution :: pell_equation:solution(),
              N :: pos_integer(),
              MaxPerimeter :: pos_integer(),
              Sum :: non_neg_integer()) -> integer().
process({X, _Y}, FirstSolution, N, MaxPerimeter, Sum) ->
    %% X = 3*A+-2
    %% X1 = 3*A + 2
    %% A1 = (X1 - 2) div 3,
    %% C1 = 2 * A1 + 1,
    %% P1 = 2 * C1 + 2 * A1,
    P1 = calc_perimeter(X, 1),
    %% X1 = 3*A + 2
    %% A2 = (X2 + 2) div 3,
    %% C2 = 2 * A1 - 1,
    %% P2 = 2 * C2 + 2 * A2,
    P2 = calc_perimeter(X, -1),
    if
        (P1 > MaxPerimeter) and (P2 > MaxPerimeter) -> Sum;
        (P1 =< MaxPerimeter) and (P2 > MaxPerimeter) -> Sum + P1;
        (P1 > MaxPerimeter) and (P2 =< MaxPerimeter) -> Sum + P2;
        true -> process(pell_equation:find_n_solution(FirstSolution, ?D, 1, N + 1), FirstSolution, N + 1, MaxPerimeter, Sum + P1 + P2)
    end.

-spec calc_perimeter(X :: integer(), Sign :: 1 | -1) -> integer().
calc_perimeter(X, Sign) ->
    %% Not all values of X are leads to the integer values of sides
    Value = X - 2 * Sign,
    if
        (Value == 0) or (Value rem 3 /= 0) -> 0;
        true ->
            A = Value div 3,
            C = 2 * A + Sign,
            2 * (C + A)
    end.