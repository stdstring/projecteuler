%% @author std-string

%% Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
%% If d(a) = b and d(b) = a, where a != b, then a and b are an amicable pair and each of a and b are called amicable numbers.
%% For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
%% The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
%% Evaluate the sum of all the amicable numbers under 10000.

-module(problem_021).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type storage() :: array:array(DividersSum :: pos_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{10000 - 1, 31626}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    DividerSumStorage = prepare_dividers_sum(MaxNumber),
    IterationFun = fun(Number, NumberSum) ->
        PairNumber = get_number_dividers_sum(Number, MaxNumber, DividerSumStorage),
        PairNumberDividersSum = get_number_dividers_sum(PairNumber, MaxNumber, DividerSumStorage),
        if
            (Number == PairNumberDividersSum) and (Number /= PairNumber) -> NumberSum + Number;
            true -> NumberSum
        end
    end,
    control:for(1, MaxNumber, 0, IterationFun).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_number_dividers_sum(Number :: pos_integer(), _MaxNumber :: pos_integer(), DividerSumStorage :: storage()) -> pos_integer().
get_number_dividers_sum(Number, MaxNumber, _DividerSumStorage) when Number > MaxNumber -> 0;
get_number_dividers_sum(Number, _MaxNumber, DividerSumStorage) -> array:get(Number - 1, DividerSumStorage).

-spec prepare_dividers_sum(MaxNumber :: pos_integer()) -> storage().
prepare_dividers_sum(MaxNumber) -> array:from_list([1] ++ [lists:sum(number_dividers:get_dividers(Number)) - Number || Number <- lists:seq(2, MaxNumber)]).