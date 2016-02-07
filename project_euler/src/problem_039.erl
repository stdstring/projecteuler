%% If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
%% {20,48,52}, {24,45,51}, {30,40,50}
%% For which value of p <= 1000, is the number of solutions maximised?

-module(problem_039).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{1000, 840}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxPerimeter) ->
    {DestPerimeter, _DestCount} = lists:foldl(fun(Current, {Perimeter, MaxCount}) ->
        CurrentCount = length(solve_right_triangle(Current)),
        if
            CurrentCount > MaxCount -> {Current, CurrentCount};
            CurrentCount =< MaxCount -> {Perimeter, MaxCount}
        end
    end, {0, 0}, lists:seq(1, MaxPerimeter)),
    DestPerimeter.

solve_right_triangle(Perimeter) ->
    [{A, B, C} || A <- lists:seq(1, Perimeter),
                  B <- lists:seq(1, Perimeter - A),
                  C <- [Perimeter - A - B],
                  C > 0,
                  A =< B,
                  A + B > C,
                  A*A + B*B == C*C].