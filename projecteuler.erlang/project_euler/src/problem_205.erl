%% @author std-string

%% Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
%% Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.
%% Peter and Colin roll their dice and compare totals: the highest total wins. The result is a draw if the totals are equal.
%% What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer rounded to seven decimal places in the form 0.abcdefg

-module(problem_205).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type dice() :: numbers:digits().
-type dice_array() :: array:array(numbers:digit()).
-type dice_roll() :: pos_integer().
-type dices_roll_state() :: {DicesRollSum :: pos_integer(), DicesRoll :: [dice_roll()]}.
-type dices_roll_data() :: dict:dict(DicesRollSum :: pos_integer(), SortedStateIndex :: non_neg_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{{[1, 2, 3, 4], 1, [1, 2, 3, 4, 5, 6], 1}, "0.2500000"},
     {{[1, 2, 3, 4], 3, [1, 2, 3, 4, 5, 6], 2}, "0.5000000"},
     {{[1, 2, 3, 4], 9, [1, 2, 3, 4, 5, 6], 6}, "0.5731441"}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({FirstDice, FirstCount, SecondDice, SecondCount}) ->
    Result = process(FirstDice, FirstCount, SecondDice, SecondCount),
    TotalCount = numbers:power(length(FirstDice), FirstCount) * numbers:power(length(SecondDice), SecondCount),
    lists:flatten(io_lib:format("~.7f", [Result / TotalCount])).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Description:
%% Let we have a set of N identical dice (with K faces), then State - the result of some roll of all these dices.
%% Let L - the set of all possible states for a set of N identical dice.
%% For each element (state) of L, we calculate the sum of the values of the roll of all dice and
%% sort the states in the set L according to the value of the sum in ascending order - get an ordered set L '.
%% It is easy to show that the values of sum for all states are in the range [N, N * K] without any holes.
%% Then, for any value, we can easily find out how many states have a sum less than this value (using an ordered set of L ').

-spec process(FirstDice :: dice(), FirstDiceCount :: pos_integer(), SecondDice :: dice(), SecondDiceCount :: pos_integer()) -> non_neg_integer().
process(FirstDice, FirstDiceCount, SecondDice, SecondDiceCount) ->
    SecondStates = generate_roll_states(array:from_list(SecondDice), SecondDiceCount),
    SecondData = prepare_roll_data(SecondStates),
    FirstStateCount = numbers:power(length(FirstDice), FirstDiceCount),
    process_impl(array:from_list(FirstDice), FirstDiceCount, 0, FirstStateCount, SecondData, 0).


-spec process_impl(FirstDice :: dice_array(),
                   FirstDiceCount :: pos_integer(),
                   FirstStateIndex :: non_neg_integer(),
                   FirstStateCount :: pos_integer(),
                   SecondData :: dices_roll_data(),
                   Result :: non_neg_integer()) -> non_neg_integer().
process_impl(_FirstDice, _FirstDiceCount, FirstStateIndex, FirstStateCount, _SecondData, Result) when FirstStateIndex == FirstStateCount -> Result;
process_impl(FirstDice, FirstDiceCount, FirstStateIndex, FirstStateCount, SecondData, Result) ->
    {FirstSum, _FirstState} = generate_roll_state(FirstDice, FirstDiceCount, FirstStateIndex),
    SecondIndex = dict:fetch(FirstSum, SecondData),
    process_impl(FirstDice, FirstDiceCount, FirstStateIndex + 1, FirstStateCount, SecondData, Result + SecondIndex).

-spec prepare_roll_data(States :: [dices_roll_state()]) -> dices_roll_data().
prepare_roll_data(States) ->
    FoldFun = fun({Sum, _State}, {Data, Index}) ->
        case dict:is_key(Sum, Data) of
            true -> {Data, Index + 1};
            false -> {dict:store(Sum, Index, Data), Index + 1}
        end
    end,
    {DestData, _DestIndex} = lists:foldl(FoldFun, {dict:new(), 0}, lists:sort(States)),
    DestData.

-spec generate_roll_states(Dice :: dice_array(), DiceCount :: pos_integer()) -> [dices_roll_state()].
generate_roll_states(Dice, DiceCount) ->
    StateCount = numbers:power(array:size(Dice), DiceCount),
    lists:map(fun(StateIndex) -> generate_roll_state(Dice, DiceCount, StateIndex) end, lists:seq(0, StateCount - 1)).

-spec generate_roll_state(Dice :: dice(), DiceCount :: pos_integer(), StateIndex :: non_neg_integer()) -> dices_roll_state().
generate_roll_state(Dice, DiceCount, StateIndex) -> generate_roll_state_impl(Dice, 0, DiceCount, StateIndex, []).

-spec generate_roll_state_impl(Dice :: dice_array(),
                               DiceIndex :: non_neg_integer(),
                               DiceCount :: pos_integer(),
                               StateIndex :: non_neg_integer(),
                               State :: [dice_roll()]) -> dices_roll_state().
generate_roll_state_impl(_Dice, DiceIndex, DiceCount, _StateIndex, State) when DiceIndex == DiceCount -> {lists:sum(State), State};
generate_roll_state_impl(Dice, DiceIndex, DiceCount, StateIndex, State) ->
    NumberIndex = StateIndex rem array:size(Dice),
    Number = array:get(NumberIndex, Dice),
    generate_roll_state_impl(Dice, DiceIndex + 1, DiceCount, StateIndex div array:size(Dice), [Number] ++ State).