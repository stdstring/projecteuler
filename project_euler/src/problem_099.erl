%% @author std-string

%% Comparing two numbers written in index form like 2^11 and 3^7 is not difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
%% However, confirming that 632382^518061 > 519432^525806 would be much more difficult, as both numbers contain over three million digits.
%% Using problem_099.dat, a data file containing one thousand lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.
%% NOTE: The first two lines in the file represent the numbers in the example given above.

-module(problem_099).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_099.dat", 709}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), ",").

-spec solve(PreparedInput :: term()) -> term().
solve(PairTable) ->
    CollectFun = fun([Base, Exponent], {Index, SavedExponentValue, SavedIndex}) ->
        ExponentValue = calculate_standard_exponent(Base, Exponent),
        if
            SavedExponentValue < ExponentValue -> {Index + 1, ExponentValue, Index};
            true -> {Index + 1, SavedExponentValue, SavedIndex}
        end
    end,
    {_Index, _SavedExponentValue, SavedIndex} = lists:foldl(CollectFun, {1, 0, -1}, PairTable),
    SavedIndex.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec calculate_standard_exponent(Base :: integer(), Exponent :: integer()) -> float().
calculate_standard_exponent(Base, Exponent) -> Exponent * math:log(Base).