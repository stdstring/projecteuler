%% @author std-string

%% It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
%% Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

-module(problem_052).

-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type search_result() :: {'true', Number :: pos_integer()} | 'false'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 142857}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) -> search(1).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec search(DigitCount :: pos_integer()) -> pos_integer().
search(DigitCount) ->
    MinNumber = numbers:power(10, DigitCount - 1),
    MaxNumber = (numbers:power(10, DigitCount) - 1) div 6,
    case search(MinNumber, MaxNumber) of
        {true, SearchedNumber} -> SearchedNumber;
        false -> search(DigitCount + 1)
    end.

-spec search(CurrentNumber :: pos_integer(), MaxNumber :: pos_integer()) -> search_result().
search(CurrentNumber, MaxNumber) when MaxNumber < CurrentNumber -> false;
search(CurrentNumber, MaxNumber) ->
    case check_number(CurrentNumber, get_result_numbers(CurrentNumber)) of
        true -> {true, CurrentNumber};
        false -> search(MaxNumber, CurrentNumber + 1)
    end.

-spec check_number(Number :: pos_integer(), ExpectedResult :: [pos_integer()]) -> boolean().
check_number(Number, ExpectedResult) ->
    NumberDigits = lists:sort(numbers:get_digits(Number)),
    lists:all(fun(ExpectedNumber) -> NumberDigits == lists:sort(ExpectedNumber) end, ExpectedResult).

-spec get_result_numbers(Number :: pos_integer()) -> [pos_integer()].
get_result_numbers(Number) -> [numbers:get_digits(Number * Factor) || Factor <- lists:seq(2, 6)].