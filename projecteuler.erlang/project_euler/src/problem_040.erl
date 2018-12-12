%% @author std-string

%% An irrational decimal fraction is created by concatenating the positive integers:
%% 0.123456789101112131415161718192021...
%% It can be seen that the 12th digit of the fractional part is 1.
%% If dn represents the nth digit of the fractional part, find the value of the following expression.
%% d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000

-module(problem_040).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 210}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) -> calculate_digit(1) *
               calculate_digit(10) *
               calculate_digit(100) *
               calculate_digit(1000) *
               calculate_digit(10000) *
               calculate_digit(100000) *
               calculate_digit(1000000).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec calculate_digit(Number :: pos_integer()) -> numbers:digit().
calculate_digit(Number) -> calculate_digit(Number, 1, 9, 1).

-spec calculate_digit(Number :: pos_integer(),
                      RangeStart :: pos_integer(),
                      RangeCount :: pos_integer(),
                      IntegerLength :: pos_integer()) -> numbers:digit().
calculate_digit(Number, RangeStart, RangeCount, IntegerLength) when Number > RangeCount * IntegerLength ->
    calculate_digit(Number - RangeCount * IntegerLength, RangeStart * 10, RangeCount * 10, IntegerLength + 1);
calculate_digit(Number, 1, 9, 1) -> Number;
calculate_digit(Number, RangeStart, _, IntegerLength) ->
    Integer = RangeStart + (Number div IntegerLength),
    LeftwardDigitPosition = (Number rem IntegerLength) - 1,
    get_digit(Integer, IntegerLength - LeftwardDigitPosition).

-spec get_digit(Number :: pos_integer(), DigitPosition :: pos_integer()) -> numbers:digit().
get_digit(Number, DigitPosition) -> (Number rem numbers:power(10, DigitPosition)) div numbers:power(10, DigitPosition - 1).