%% @author std-string

%% Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of numbers less than n which are relatively prime to n.
%% For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
%% n = 2,  Relatively Prime = 1,             φ(n) = 1, n/φ(n) = 2
%% n = 3,  Relatively Prime = {1,2},         φ(n) = 2, n/φ(n) = 1.5
%% n = 4,  Relatively Prime = {1,3},         φ(n) = 2, n/φ(n) = 2
%% n = 5,  Relatively Prime = {1,2,3,4},     φ(n) = 4, n/φ(n) = 1.25
%% n = 6,  Relatively Prime = {1,5},         φ(n) = 2, n/φ(n) = 3
%% n = 7,  Relatively Prime = {1,2,3,4,5,6}, φ(n) = 6, n/φ(n) = 1.1666...
%% n = 8,  Relatively Prime = {1,3,5,7},     φ(n) = 4, n/φ(n) = 2
%% n = 9,  Relatively Prime = {1,2,4,5,7,8}, φ(n) = 6, n/φ(n) = 1.5
%% n = 10, Relatively Prime = {1,3,7,9},     φ(n) = 4, n/φ(n) = 2.5
%% It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.
%% Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

-module(problem_069).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{10, 6}, {1000000, 510510}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    Storage = number_dividers:create_prime_dividers(MaxNumber),
    {ResultIndex, _MaxValue} = array:foldl(fun(Index, Dividers, {SavedIndex, SavedValue}) ->
        Number = Index + 2,
        Value = Number / number_dividers:calc_euler_function(Number, sets:to_list(Dividers)),
        if
            Value > SavedValue -> {Index, Value};
            true -> {SavedIndex, SavedValue}
        end
    end, {-1, 1}, Storage),
    ResultIndex + 2.

%% ====================================================================
%% Internal functions
%% ====================================================================