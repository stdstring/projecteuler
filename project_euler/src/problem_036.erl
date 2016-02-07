%% The decimal number, 585 (base 10) = 1001001001 (base 2), is palindromic in both bases.
%% Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
%% (Please note that the palindromic number, in either base, may not include leading zeros.)

-module(problem_036).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{999999, 872187}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) ->
    %% Digits allways bottom half of number's digits
    %% e.g, for number 67576 Digits = [5, 7, 6], for number 675576 Digits = [5, 7, 6]
    Digits = array:set(0, 0, array:new()),
    process_number(MaxNumber, Digits, 1, 0).

-spec process_number(MaxNumber :: pos_integer(), Digits :: array:array(non_neg_integer()), DigitsCount :: pos_integer(), Result :: non_neg_integer()) -> non_neg_integer().
process_number(MaxNumber, Digits, DigitsCount, Result) ->
    {NewDigits, NewDigitsCount} = update_digits(Digits, DigitsCount),
    Number = create_number(NewDigits, NewDigitsCount),
    if
        Number > MaxNumber -> Result;
        Number =< MaxNumber ->
            case check_number(Number) of
                true -> process_number(MaxNumber, NewDigits, NewDigitsCount, Result + Number);
                false -> process_number(MaxNumber, NewDigits, NewDigitsCount, Result)
            end
    end.

-spec update_digits(Digits :: array:array(non_neg_integer()), DigitsCount :: pos_integer()) -> array:array(non_neg_integer()).
update_digits(Digits, DigitsCount) ->
    MaxIndex = array:size(Digits) - 1,
    update_digits(Digits, DigitsCount, 0, MaxIndex).

-spec update_digits(Digits :: array:array(non_neg_integer()), DigitsCount :: pos_integer(), Index :: non_neg_integer(), MaxIndex :: non_neg_integer()) -> array:array(non_neg_integer()).
update_digits(Digits, DigitsCount, Index, MaxIndex) when Index > MaxIndex ->
    DigitsCountRem = DigitsCount rem 2,
    if
        DigitsCountRem == 0 -> {array:set(Index, 1, Digits), DigitsCount + 1};
        DigitsCountRem /= 0 -> {array:set(MaxIndex, 1, Digits), DigitsCount + 1}
    end;
update_digits(Digits, DigitsCount, Index, MaxIndex) ->
    Digit = array:get(Index, Digits),
    if
        Digit < 9 -> {array:set(Index, Digit + 1, Digits), DigitsCount};
        Digit == 9 -> update_digits(array:set(Index, 0, Digits), DigitsCount, Index + 1, MaxIndex)
    end.

-spec create_number(Digits :: array:array(non_neg_integer()), DigitsCount :: pos_integer()) -> pos_integer().
create_number(Digits, DigitsCount) ->
    InnerDigit = array:get(0, Digits),
    DigitsCountRem = DigitsCount rem 2,
    if
        DigitsCountRem == 0 -> numbers:get_number(create_number_impl(Digits, 1, (DigitsCount div 2) - 1, [InnerDigit, InnerDigit]));
        DigitsCountRem /= 0 -> numbers:get_number(create_number_impl(Digits, 1, DigitsCount div 2, [InnerDigit]))
    end.

-spec create_number_impl(Digits :: array:array(non_neg_integer()), Index :: non_neg_integer(), MaxIndex :: non_neg_integer(), Result:: [non_neg_integer()]) -> [non_neg_integer()].
create_number_impl(_Digits, Index, MaxIndex, Result) when Index > MaxIndex -> Result;
create_number_impl(Digits, Index, MaxIndex, Result) ->
    Digit = array:get(Index, Digits),
    create_number_impl(Digits, Index + 1, MaxIndex, [Digit] ++ Result ++ [Digit]).

-spec check_number(Number :: pos_integer()) -> boolean().
check_number(Number) ->
    Digits = numbers:get_digits(Number, 2),
    ReversedDigits = lists:reverse(Digits),
    Digits == ReversedDigits.