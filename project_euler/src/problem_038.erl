%% Take the number 192 and multiply it by each of 1, 2, and 3:
%% 192 * 1 = 192
%% 192 * 2 = 384
%% 192 * 3 = 576
%% By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
%% The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
%% What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

-module(problem_038).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{none, 932718654}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) -> process_number(9, []).

-spec process_number(Number :: pos_integer(), SavedDigits :: [0..9]) -> pos_integer().
process_number(10000, SavedDigits) -> numbers:get_number(SavedDigits);
process_number(1000, SavedDigits) -> process_number(9000, SavedDigits);
process_number(100, SavedDigits) -> process_number(900, SavedDigits);
process_number(10, SavedDigits) -> process_number(90, SavedDigits);
process_number(Number, SavedDigits) ->
    case process_number(Number, 1, [], 0) of
        false -> process_number(Number + 1, SavedDigits);
        {true, Digits} ->
            if
                SavedDigits < Digits -> process_number(Number + 1, Digits);
                true -> process_number(Number + 1, SavedDigits)
            end
    end.

-spec process_number(Number :: pos_integer(), Factor :: pos_integer(), Digits :: [0..9], DigitsCount :: non_neg_integer()) ->
    {'true', Digits :: [0..9]} | 'false'.
process_number(_Number, _Factor, _Digits, DigitsCount) when DigitsCount > 9 -> false;
process_number(_Number, _Factor, Digits, DigitsCount) when DigitsCount == 9 ->
    case pandigital_numbers:is_pandigital(Digits) of
        true -> {true, Digits};
        false -> false
    end;
process_number(Number, Factor, Digits, DigitsCount) ->
    Result = Number * Factor,
    ResultDigits = numbers:get_digits(Result),
    ResultDigitsCount = length(ResultDigits),
    process_number(Number, Factor + 1, Digits ++ ResultDigits, DigitsCount + ResultDigitsCount).