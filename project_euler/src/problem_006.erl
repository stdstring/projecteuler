%% The sum of the squares of the first ten natural numbers is, 1^2 + 2^2 + ... + 10^2 = 385
%% The square of the sum of the first ten natural numbers is, (1 + 2 + ... + 10)^2 = 55^2 = 3025
%% Hence the difference between the sum of the squares of the first ten natural numbers
%% and the square of the sum is 3025 - 385 = 2640.
%% Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-module(problem_006).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{10, 2640}, {100, 25164150}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) ->
    Sum = (1 + MaxNumber) * MaxNumber div 2,
    SquareSum = Sum * Sum,
    SumSquare = calc_sum_square(MaxNumber),
    SquareSum - SumSquare.

calc_sum_square(MaxNumber) ->
    calc_sum_square(1, MaxNumber, 0).

calc_sum_square(MaxNumber, MaxNumber, Sum) ->
    Sum + MaxNumber * MaxNumber;
calc_sum_square(Number, MaxNumber, Sum) ->
    calc_sum_square(Number + 1, MaxNumber, Sum + Number * Number).