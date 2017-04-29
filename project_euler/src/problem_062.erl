%% @author std-string

%% The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
%% In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
%% Find the smallest cube for which exactly five permutations of its digits are cube.

-module(problem_062).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(INIT_NUMBER, 1).
-define(INIT_CUBE_SUP, 10).

-type cube_dictionary() :: dict:dict(numbers:digits(), [Number :: pos_integer()]).

-record(state, {cube_numbers :: cube_dictionary(), suitable_keys :: [numbers:digits()]}).

-type range_result() :: {Number :: pos_integer(), State :: #state{}}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{3, 41063625}, {5, 127035954683}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(PermutationsCount) -> process_numbers(?INIT_NUMBER, ?INIT_CUBE_SUP, PermutationsCount).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(Number :: pos_integer(), CubeSup :: pos_integer(), PermutationsCount :: pos_integer()) -> pos_integer().
process_numbers(Number, CubeSup, PermutationsCount) ->
    State = #state{cube_numbers = dict:new(), suitable_keys = []},
    {NextNumber, NewState} = process_range(Number, CubeSup, State, PermutationsCount),
    case NewState#state.suitable_keys of
        [] -> process_numbers(NextNumber, CubeSup * 10, PermutationsCount);
        SuitableKeys -> process_suiatble(SuitableKeys, NewState#state.cube_numbers, undefined)
    end.

-spec process_range(Number :: pos_integer(), CubeSup :: pos_integer(), State :: #state{}, PermutationsCount :: pos_integer()) -> range_result().
process_range(Number, CubeSup, State, PermutationsCount) ->
    CubeValue = Number * Number * Number,
    if
        CubeValue =< CubeSup -> process_range(Number + 1, CubeSup, process_number(CubeValue, State, PermutationsCount), PermutationsCount);
        CubeValue > CubeSup -> {Number, State}
    end.

-spec process_number(Number :: pos_integer(), State :: #state{}, PermutationsCount :: pos_integer()) -> #state{}.
process_number(Number, State, PermutationsCount) ->
    Digits = lists:sort(numbers:get_digits(Number)),
    CubeNumbersDict = dict:append(Digits, Number, State#state.cube_numbers),
    SuitableKeys = State#state.suitable_keys,
    NumbersCount = length(dict:fetch(Digits, CubeNumbersDict)),
    if
        NumbersCount < PermutationsCount -> State#state{cube_numbers = CubeNumbersDict};
        NumbersCount == PermutationsCount -> State#state{cube_numbers = CubeNumbersDict, suitable_keys = [Digits] ++ SuitableKeys};
        NumbersCount > PermutationsCount -> State#state{cube_numbers = CubeNumbersDict, suitable_keys = SuitableKeys -- [Digits]}
    end.

-spec process_suiatble(SuitableKeys :: [numbers:digits()],
                       CubeNumbers :: cube_dictionary(),
                       MinValue :: 'undefined' | pos_integer()) -> pos_integer().
process_suiatble([], _CubeNumbers, MinValue) -> MinValue;
process_suiatble([Key | KeysRest], CubeNumbers, undefined) ->
    process_suiatble(KeysRest, CubeNumbers, lists:min(dict:fetch(Key, CubeNumbers)));
process_suiatble([Key | KeysRest], CubeNumbers, MinValue) ->
    CurrentMinValue = lists:min(dict:fetch(Key, CubeNumbers)),
    if
        CurrentMinValue < MinValue -> process_suiatble(KeysRest, CubeNumbers, CurrentMinValue);
        CurrentMinValue >= MinValue -> process_suiatble(KeysRest, CubeNumbers, MinValue)
    end.