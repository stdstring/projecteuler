%% @author std-string

%% The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
%% Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

-module(problem_048).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{10, 10}, "0405071317"}, {{1000, 10}, "9110846700"}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxNumber, LastDigitCount}) ->
    Numbers = lists:seq(1, MaxNumber),
    Divider = numbers:power(10, LastDigitCount),
    TransformNumbers = lists:map(fun(Number) -> numbers:power(Number, Number) rem Divider end, Numbers),
    Sum = integer_to_list(lists:sum(TransformNumbers) rem Divider),
    lists:duplicate(LastDigitCount - length(Sum), $0) ++ Sum.

%% ====================================================================
%% Internal functions
%% ====================================================================