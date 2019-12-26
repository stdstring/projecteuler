%% @author std-string

%% If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, and two discs were taken at random,
%% it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.
%% The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random,
%% is a box containing eighty-five blue discs and thirty-five red discs.
%% By finding the first arrangement to contain over 1000000000000 discs in total, determine the number of blue discs that the box would contain.

-module(problem_100).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(D, 2).
-define(C, -1).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{21, 15}, {22, 85}, {1000000000000, 756872327473}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MinTotalCount) ->
    %% P(BB) = (Nb / T) * ((Nb - 1) / (T - 1)) = (Nb^2 - Nb) / (T^2 - T) = (Nb^2 - Nb + 1/4 - 1/4) / (T^2 - T + 1/4 - 1/4)
    %% P(BB) = 1/2 => 2 * (Nb^2 - Nb + 1/4 - 1/4) = (T^2 - T + 1/4 - 1/4)
    %% 2 * (Nb - 1/2)^2 - 1/2 = (T - 1/2)^2 - 1/4 => (T - 1/2)^2 - 2 * (Nb - 1/2)^2 = -1/4 =>
    %% 4 * (T - 1/2)^2 - 4 * 2 * (Nb - 1/2)^2 = -1 => (2 * T - 1)^2 - 2 * (2 * Nb - 1)^2 = -1
    %% X = 2 * T - 1, Y = 2 * Nb - 1 => X^2 - 2 * Y^2 = -1
    %% X = 2 * T - 1, Y = 2 * B - 1 => X & Y must be odd
    FirstSolution = pell_equation:find_first_solution(?D, ?C),
    process(FirstSolution, 1, MinTotalCount).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(FirstSolution :: pell_equation:solution(), N :: pos_integer(), MinTotalCount :: pos_integer()) -> pos_integer().
process(FirstSolution, N, MinTotalCount) ->
    {X, Y} = pell_equation:find_n_solution(FirstSolution, ?D, ?C, N),
    TotalCount = (X + 1) div 2,
    if
        TotalCount < MinTotalCount -> process(FirstSolution, N + 2, MinTotalCount);
        TotalCount >= MinTotalCount -> (Y + 1) div 2
    end.