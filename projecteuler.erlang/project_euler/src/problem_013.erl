%% @author std-string

%% Work out the first ten digits of the sum of the one-hundred 50-digit numbers in "problem_013.dat".

-module(problem_013).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{"problem_013.dat", 10}, "5537376230"}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, {Filename, DigitsCount}) ->
    NumberStrings = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    Numbers = lists:map(fun list_to_integer/1, NumberStrings),
    {Numbers, DigitsCount}.

-spec solve(PreparedInput :: term()) -> term().
solve({Numbers, DigitsCount}) ->
    Sum = lists:sum(Numbers),
    SumString = integer_to_list(Sum),
    lists:sublist(SumString, DigitsCount).

%% ====================================================================
%% Internal functions
%% ====================================================================