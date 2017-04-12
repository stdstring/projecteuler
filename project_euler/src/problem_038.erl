%% @author std-string

%% Take the number 192 and multiply it by each of 1, 2, and 3:
%% 192 * 1 = 192
%% 192 * 2 = 384
%% 192 * 3 = 576
%% By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
%% The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
%% What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

-module(problem_038).
-export([get_check_data/0, prepare_data/2, solve/1]).

-include("pandigital_def.hrl").

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{none, 932718654}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) -> process_number(9, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_number(Number :: pos_integer(), SavedResult :: non_neg_integer()) -> pos_integer().
process_number(10000, SavedResult) -> SavedResult;
process_number(1000, SavedResult) -> process_number(9000, SavedResult);
process_number(100, SavedResult) -> process_number(900, SavedResult);
process_number(10, SavedResult) -> process_number(90, SavedResult);
process_number(Number, SavedResult) ->
    case process_number(Number, 1, 0) of
        false -> process_number(Number + 1, SavedResult);
        {true, Result} ->
            if
                SavedResult < Result ->  process_number(Number + 1, Result);
                true -> process_number(Number + 1, SavedResult)
            end
    end.

-spec process_number(Number :: pos_integer(), Factor :: 1..9, Result :: non_neg_integer()) -> {'true', Result :: pos_integer()} | 'false'.
process_number(_Number, _Factor, Result) when Result > ?PANDIGITAL9_MAX -> false;
process_number(_Number, _Factor, Result) when (Result >= ?PANDIGITAL9_MIN) and (Result =< ?PANDIGITAL9_MAX) ->
    case pandigital_numbers:is_pandigital(Result) of
        true -> {true, Result};
        false -> false
    end;
process_number(Number, Factor, Result) ->
    Value = Number * Factor,
    Multiplier = calc_multiplier(Value, 1),
    process_number(Number, Factor + 1, Result * Multiplier + Value).

-spec calc_multiplier(Value :: pos_integer(), Multiplier :: pos_integer()) -> pos_integer().
calc_multiplier(Value, Multiplier) when Value < Multiplier -> Multiplier;
calc_multiplier(Value, Multiplier) -> calc_multiplier(Value, Multiplier * 10).