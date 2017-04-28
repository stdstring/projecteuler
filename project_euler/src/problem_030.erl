%% @author std-string

%% Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
%% 1634 = 1^4 + 6^4 + 3^4 + 4^4
%% 8208 = 8^4 + 2^4 + 0^4 + 8^4
%% 9474 = 9^4 + 4^4 + 7^4 + 4^4
%% As 1 = 1^4 is not a sum it is not included.
%% The sum of these numbers is 1634 + 8208 + 9474 = 19316.
%% Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

-module(problem_030).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type power_storage() :: array:array(DigitPower :: non_neg_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{4, 19316}, {5, 443839}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Power) ->
    Delta = numbers:power(9, Power),
    Infimum = find_infimum(Delta),
    {Power, Infimum}.

-spec solve(PreparedInput :: term()) -> term().
solve({Power, Infimum}) ->
    PowerStorage = prepare_powers(9, Power),
    traverse_numbers(Infimum, PowerStorage).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_infimum(Delta :: pos_integer()) -> pos_integer().
find_infimum(Delta) -> find_infimum(1, 10, Delta, Delta).

-spec find_infimum(LeftBound :: pos_integer(), RightBound :: pos_integer(), Value  :: pos_integer(), Delta :: pos_integer()) -> pos_integer().
find_infimum(LeftBound, RightBound, Value, _Delta) when (LeftBound < Value) and (Value < RightBound) -> Value;
find_infimum(LeftBound, RightBound, Value, Delta) -> find_infimum(10 * LeftBound, 10 * RightBound, Value + Delta, Delta).

-spec traverse_numbers(Max :: pos_integer(), PowerStorage :: power_storage()) -> pos_integer().
traverse_numbers(Max, PowerStorage) -> traverse_numbers(2, Max, 0, PowerStorage).

-spec traverse_numbers(Current :: pos_integer(), Max :: pos_integer(), Sum :: non_neg_integer(), PowerStorage :: power_storage()) -> pos_integer().
traverse_numbers(Current, Max, Sum, _) when Current > Max -> Sum;
traverse_numbers(Current, Max, Sum, PowerStorage) ->
    DigitsPowerSum = lists:foldl(fun(Digit, Acc) -> Acc + array:get(Digit, PowerStorage) end, 0, numbers:get_digits(Current)),
    if
        Current == DigitsPowerSum -> traverse_numbers(Current + 1, Max, Sum + Current, PowerStorage);
        Current /= DigitsPowerSum -> traverse_numbers(Current + 1, Max, Sum, PowerStorage)
    end.

-spec prepare_powers(MaxNumber :: pos_integer(), Power :: pos_integer()) -> power_storage().
prepare_powers(MaxNumber, Power) -> array:from_list([numbers:power(Number, Power) || Number <- lists:seq(0, MaxNumber)]).