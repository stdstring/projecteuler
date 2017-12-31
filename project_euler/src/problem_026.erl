%% @author std-string

%% A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
%% 1/2 = 0.5
%% 1/3 = 0.(3)
%% 1/4 = 0.25
%% 1/5 = 0.2
%% 1/6 = 0.1(6)
%% 1/7 = 0.(142857)
%% 1/8 = 0.125
%% 1/9 = 0.(1)
%% 1/10 = 0.1
%% Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
%% Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

-module(problem_026).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type process_result() :: {SavedNumber :: pos_integer(), SavedCycleLength :: non_neg_integer()}.
-type process_number_result() :: {'true', CycleLength :: non_neg_integer()} | 'false'.
-type remainder_array() :: array:array(Remainder :: non_neg_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{10, 7}, {1000, 983}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

%% Solution:
%% 1) For number N there are N distinct rests: 0, 1, ..., N-1
%% 2) Let Lp - length of non recurring prefix, Lc - length of recurring cycle in the decimal fraction part
%% For number N : 10^n < N < 10^(n + 1) we can state the following:
%% Rem1 = 10^(n + 1) rem N, Rem2 = 10^(n + 2) rem N, ...Remp = 10^(n + Lp) rem N, RemC1 = 10^(n + Lp + 1) rem N, ..., RemCN = 10^(n + Lp + Lc) rem N
%% Rem1 /= Rem2 /= ..., RemC1 == RemCn, Lp + Lc < = N
-spec solve(PreparedInput :: term()) -> term().
solve(MaxDenominator) ->
    {Number, _CycleLength} = process_numbers(MaxDenominator, 10, {1, 0}),
    Number.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(Number :: pos_integer(), Numerator :: pos_integer(), PrevValue :: process_result()) -> process_result().
process_numbers(1, _Numerator, {SavedNumber, SavedCycleLength}) -> {SavedNumber, SavedCycleLength};
process_numbers(Number, _Numerator, {SavedNumber, SavedCycleLength}) when SavedCycleLength > Number -> {SavedNumber, SavedCycleLength};
process_numbers(Number, Numerator, {SavedNumber, SavedCycleLength}) ->
    case process_number(Number, Numerator) of
        false -> process_numbers(Number - 1, 10 * Numerator, {SavedNumber, SavedCycleLength});
        {true, CycleLength} when CycleLength > SavedCycleLength -> process_numbers(Number - 1, Numerator, {Number, CycleLength});
        {true, _CycleLength} -> process_numbers(Number - 1, Numerator, {SavedNumber, SavedCycleLength})
    end.

-spec process_number(Number :: pos_integer(), Numerator :: pos_integer()) -> process_number_result().
process_number(Numerator, Numerator) -> false;
process_number(Number, Numerator) when Numerator rem Number == 0 -> {true, 0};
process_number(Number, Numerator) ->
    RemArray = array:new([{fixed, true}, {size, Number + 1}, {default, undefined}]),
    CycleLength = process_number(Number, Numerator, 0, RemArray),
    {true, CycleLength}.

-spec process_number(Number :: pos_integer(),
                     Numerator :: pos_integer(),
                     Index :: non_neg_integer(),
                     RemArray :: remainder_array()) -> non_neg_integer().
process_number(Number, Numerator, Index, RemArray) ->
    RemValue = Numerator rem Number,
    case array:get(RemValue, RemArray) of
        undefined -> process_number(Number, 10 * Numerator, Index + 1, array:set(RemValue, Index, RemArray));
        RestIndex -> Index - RestIndex
    end.