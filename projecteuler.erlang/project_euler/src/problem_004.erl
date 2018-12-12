%% @author std-string

%% A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.
%% Find the largest palindrome made from the product of two 3-digit numbers.

-module(problem_004).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(PORTION_DIVIDER, 10).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{10, 99}, 9009}, {{100, 999}, 906609}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({NumberMin, NumberMax}) ->
    ResultNumberMax = NumberMax * NumberMax,
    Delta = ResultNumberMax div ?PORTION_DIVIDER,
    process_search(ResultNumberMax - Delta, ResultNumberMax, NumberMin, NumberMax, Delta, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_search(RangeMin :: non_neg_integer(),
                     RangeMax :: pos_integer(),
                     NumberMin :: pos_integer(),
                     NumberMax :: pos_integer(),
                     Delta :: pos_integer(),
                     Result :: non_neg_integer()) -> non_neg_integer().
process_search(_RangeMin, 0, _NumberMin, _NumberMax, _Delta, Result) -> Result;
process_search(RangeMin, RangeMax, NumberMin, NumberMax, Delta, Result) when RangeMin < 0 ->
    process_search(0, RangeMax, NumberMin, NumberMax, Delta, Result);
process_search(RangeMin, RangeMax, NumberMin, NumberMax, Delta, Result) ->
    NewResult = process_range(RangeMin, RangeMax, NumberMin, NumberMax, NumberMax, NumberMax, Result),
    if
        NewResult /= 0 -> NewResult;
        NewResult == 0 -> process_search(RangeMin - Delta, RangeMin, NumberMin, NumberMax, Delta, NewResult)
    end.

-spec process_range(RangeMin :: non_neg_integer(),
                    RangeMax :: pos_integer(),
                    NumberMin :: pos_integer(),
                    NumberMax :: pos_integer(),
                    Number1 :: pos_integer(),
                    Number2 :: pos_integer(),
                    Result :: non_neg_integer()) -> non_neg_integer().
process_range(RangeMin, _RangeMax, _NumberMin, _NumberMax, Number, Number, Result) when Number * Number < RangeMin -> Result;
process_range(_RangeMin, _RangeMax, NumberMin, _NumberMax, Number, Number, Result) when Number < NumberMin -> Result;
process_range(RangeMin, RangeMax, NumberMin, NumberMax, Number1, _Number2, Result) when Number1 * NumberMin > RangeMax ->
    process_range(RangeMin, RangeMax, NumberMin, NumberMax, Number1 - 1, Number1 - 1, Result);
process_range(RangeMin, RangeMax, NumberMin, NumberMax, Number1, Number2, Result) ->
    Production = Number1 * Number2,
    if
        Production < RangeMin -> process_range(RangeMin, RangeMax, NumberMin, NumberMax, Number1 - 1, Number1 - 1, Result);
        Production >= RangeMin -> process_number(RangeMin, RangeMax, NumberMin, NumberMax, Number1, Number2, Production, Result)
    end.

-spec process_number(RangeMin :: non_neg_integer(),
                     RangeMax :: pos_integer(),
                     NumberMin :: pos_integer(),
                     NumberMax :: pos_integer(),
                     Number1 :: pos_integer(),
                     Number2 :: pos_integer(),
                     Production :: pos_integer(),
                     Result :: non_neg_integer()) -> no_return().
process_number(RangeMin, RangeMax, NumberMin, NumberMax, Number1, Number2, Production, Result) ->
    case check_number(Production) of
        false -> process_range(RangeMin, RangeMax, NumberMin, NumberMax, Number1, Number2 - 1, Result);
        true ->
            if
                Result < Production -> process_range(RangeMin, RangeMax, NumberMin, NumberMax, Number1, Number2 - 1, Production);
                Result >= Production -> process_range(RangeMin, RangeMax, NumberMin, NumberMax, Number1, Number2 - 1, Result)
            end
    end.

-spec check_number(Number :: pos_integer()) -> boolean().
check_number(Number) ->
    Digits = numbers:get_digits(Number),
    ReversedDigits = lists:reverse(Digits),
    Digits == ReversedDigits.