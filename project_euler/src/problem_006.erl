%% @author std-string

%% The sum of the squares of the first ten natural numbers is, 1^2 + 2^2 + ... + 10^2 = 385
%% The square of the sum of the first ten natural numbers is, (1 + 2 + ... + 10)^2 = 55^2 = 3025
%% Hence the difference between the sum of the squares of the first ten natural numbers
%% and the square of the sum is 3025 - 385 = 2640.
%% Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-module(problem_006).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{10, 2640}, {100, 25164150}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    Sum = (1 + MaxNumber) * MaxNumber div 2,
    SquareSum = Sum * Sum,
    SumSquare = calc_sum_square(MaxNumber),
    SquareSum - SumSquare.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec calc_sum_square(MaxNumber :: pos_integer()) -> pos_integer().
calc_sum_square(MaxNumber) ->
    calc_sum_square(1, MaxNumber, 0).

-spec calc_sum_square(Number :: pos_integer(), MaxNumber :: pos_integer(), Sum :: non_neg_integer()) -> pos_integer().
calc_sum_square(MaxNumber, MaxNumber, Sum) ->
    Sum + MaxNumber * MaxNumber;
calc_sum_square(Number, MaxNumber, Sum) ->
    calc_sum_square(Number + 1, MaxNumber, Sum + Number * Number).