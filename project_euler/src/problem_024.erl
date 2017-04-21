%% @author std-string

%% A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
%% If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
%% The lexicographic permutations of 0, 1 and 2 are: 012, 021, 102, 120, 201, 210
%% What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

-module(problem_024).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{{4, 3}, "120"}, {{1000000, 10}, "2783915460"}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({_LexographicNumber, DigitCount}) when DigitCount > 10 ->
    throw(badarg);
solve({LexographicNumber, DigitCount}) ->
    SourceDigits = lists:seq($0, $0 + DigitCount - 1),
    Alphabet = array:from_list(SourceDigits),
    permutations:get_permutation(LexographicNumber - 1, Alphabet).

%% ====================================================================
%% Internal functions
%% ====================================================================