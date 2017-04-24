%% @author std-string

%% We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
%% The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
%% Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
%% HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

-module(problem_032).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 45228}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) -> lists:sum(sets:to_list(process_number())).

%% ====================================================================
%% Internal functions
%% ====================================================================

process_number() -> process_number([1, 2, 3, 4], sets:new()).

process_number(ResultProbe, Storage) ->
    case choose_result_number(ResultProbe) of
        stop -> Storage;
        [Digit1, Digit2, Digit3, Digit4] ->
            ResultValue = 1000 * Digit1 + 100 * Digit2 + 10 * Digit3 + Digit4,
            UpdatedStorage = process_small_factor([Digit1, Digit2, Digit3, Digit4], ResultValue, Storage),
            process_number([Digit1, Digit2, Digit3, Digit4 + 1], UpdatedStorage)
    end.

process_small_factor(Result, ResultValue, Storage) ->
    process_small_factor(Result, ResultValue, [2], Storage).

process_small_factor(Result, ResultValue, SmallFactorProbe, Storage) ->
    case choose_small_factor(Result, SmallFactorProbe) of
        stop -> Storage;
        [Digit5] -> process_small_factor(Result, ResultValue, [Digit5], Digit5, [Digit5 + 1], Storage);
        [Digit5, Digit6] -> process_small_factor(Result, ResultValue, [Digit5, Digit6], 10 * Digit5 + Digit6, [Digit5, Digit6 + 1], Storage)
    end.

process_small_factor(Result, ResultValue, SmallFactor, SmallFactorValue, NextSmallFactorProbe, Storage) ->
    case check_big_factor(Result, ResultValue, SmallFactor, SmallFactorValue) of
        false -> process_small_factor(Result, ResultValue, NextSmallFactorProbe, Storage);
        true -> process_small_factor(Result, ResultValue, NextSmallFactorProbe, sets:add_element(ResultValue, Storage))
    end.

check_big_factor(Result, ResultValue, SmallFactor, SmallFactorValue) ->
    Rem = ResultValue rem SmallFactorValue,
    if
        Rem /= 0 -> false;
        Rem == 0 ->
            BigFactorValue = ResultValue div SmallFactorValue,
            BigFactor = numbers:get_digits(BigFactorValue),
            check_big_factor(Result, SmallFactor, BigFactor)
    end.

%% unsuitable length
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit7, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit8, _Digit9]) -> false;
%% check first digit in 4-digits big factor
check_big_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5], [Digit, _Digit7, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5], [Digit, _Digit7, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5], [Digit, _Digit7, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5], [Digit, _Digit7, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit], [Digit, _Digit7, _Digit8, _Digit9]) -> false;
%% check second digit in 4-digits big factor
check_big_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5], [_Digit6, Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5], [_Digit6, Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5], [_Digit6, Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit], [_Digit6, Digit, _Digit8, _Digit9]) -> false;
%% check third digit in 4-digits big factor
check_big_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5], [_Digit6, _Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5], [_Digit6, _Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit], [_Digit6, _Digit7, Digit, _Digit9]) -> false;
%% check forth digit in 4-digits big factor
check_big_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5], [_Digit6, _Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5], [_Digit6, _Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit], [_Digit6, _Digit7, _Digit8, Digit]) -> false;
%% check duplicates in 4-digits big factor
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [Digit, Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [Digit, _Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [Digit, _Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, Digit, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, Digit, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, Digit, Digit]) -> false;
%% check first digit in 3-digits big factor
check_big_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5, _Digit6], [Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5, _Digit6], [Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5, _Digit6], [Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit, _Digit6], [Digit, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, Digit], [Digit, _Digit8, _Digit9]) -> false;
%% check second digit in 3-digits big factor
check_big_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5, _Digit6], [_Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5, _Digit6], [_Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit, _Digit6], [_Digit7, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, Digit], [_Digit7, Digit, _Digit9]) -> false;
%% check third digit in 3-digits big factor
check_big_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5, _Digit6], [_Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5, _Digit6], [_Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit, _Digit6], [_Digit7, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, Digit], [_Digit7, _Digit8, Digit]) -> false;
%% check duplicates in 3-digits big factor
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [Digit, Digit, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [Digit, _Digit8, Digit]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, Digit, Digit]) -> false;
%% check digit 0 in 4-digits big factor
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, 0, _Digit8, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, 0, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, _Digit8, 0]) -> false;
%% check digit 0 in 3-digits big factor
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, 0, _Digit9]) -> false;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, _Digit8, 0]) -> false;
%% suitable big factors
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5], [_Digit6, _Digit7, _Digit8, _Digit9]) -> true;
check_big_factor([_Digit1, _Digit2, _Digit3, _Digit4], [_Digit5, _Digit6], [_Digit7, _Digit8, _Digit9]) -> true.

choose_small_factor([_Digit1, _Digit2, _Digit3, _Digit4], [10, 1]) -> stop;
choose_small_factor([Digit1, Digit2, Digit3, Digit4], [10]) ->
    choose_small_factor([Digit1, Digit2, Digit3, Digit4], [1, 2]);
choose_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5, 10]) ->
    choose_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5 + 1, 1]);
choose_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5]) ->
    case check_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5]) of
        false -> choose_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5 + 1]);
        Result -> Result
    end;
choose_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5, Digit6]) ->
    case check_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5, Digit6]) of
        false -> choose_small_factor([Digit1, Digit2, Digit3, Digit4], [Digit5, Digit6 + 1]);
        Result -> Result
    end.

%% check digit in 1-digits small factor
check_small_factor([Digit, _Digit2, _Digit3, _Digit4], [Digit]) -> false;
check_small_factor([_Digit1, Digit, _Digit3, _Digit4], [Digit]) -> false;
check_small_factor([_Digit1, _Digit2, Digit, _Digit4], [Digit]) -> false;
check_small_factor([_Digit1, _Digit2, _Digit3, Digit], [Digit]) -> false;
%% check first digit in 2-digits small factor
check_small_factor([Digit, _Digit2, _Digit3, _Digit4], [Digit, _Digit6]) -> false;
check_small_factor([_Digit1, Digit, _Digit3, _Digit4], [Digit, _Digit6]) -> false;
check_small_factor([_Digit1, _Digit2, Digit, _Digit4], [Digit, _Digit6]) -> false;
check_small_factor([_Digit1, _Digit2, _Digit3, Digit], [Digit, _Digit6]) -> false;
%% check second digit in 2-digits small factor
check_small_factor([Digit, _Digit2, _Digit3, _Digit4], [_Digit5, Digit]) -> false;
check_small_factor([_Digit1, Digit, _Digit3, _Digit4], [_Digit5, Digit]) -> false;
check_small_factor([_Digit1, _Digit2, Digit, _Digit4], [_Digit5, Digit]) -> false;
check_small_factor([_Digit1, _Digit2, _Digit3, Digit], [_Digit5, Digit]) -> false;
%% check duplicates in 2-digits small factor
check_small_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit, Digit]) -> false;
%% suitable small factors
check_small_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit5]) -> [Digit5];
check_small_factor([_Digit1, _Digit2, _Digit3, _Digit4], [Digit5, Digit6]) -> [Digit5, Digit6].

choose_result_number([10, 1, 1, 1]) -> stop;
choose_result_number([Digit1, 10, 1, 1]) -> choose_result_number([Digit1 + 1, 1, 1, 1]);
choose_result_number([Digit1, Digit2, 10, 1]) -> choose_result_number([Digit1, Digit2 + 1, 1, 1]);
choose_result_number([Digit1, Digit2, Digit3, 10]) -> choose_result_number([Digit1, Digit2, Digit3 + 1, 1]);
choose_result_number([Digit1, Digit2, Digit3, Digit4]) ->
    case check_result_number([Digit1, Digit2, Digit3, Digit4]) of
        false -> choose_result_number([Digit1, Digit2, Digit3, Digit4 + 1]);
        Result -> Result
    end.

check_result_number([Digit, Digit, _Digit3, _Digit4]) -> false;
check_result_number([Digit, _Digit2, Digit, _Digit4]) -> false;
check_result_number([Digit, _Digit2, _Digit3, Digit]) -> false;
check_result_number([_Digit1, Digit, Digit, _Digit4]) -> false;
check_result_number([_Digit1, Digit, _Digit3, Digit]) -> false;
check_result_number([_Digit1, _Digit2, Digit, Digit]) -> false;
check_result_number([Digit1, Digit2, Digit3, Digit4]) -> [Digit1, Digit2, Digit3, Digit4].