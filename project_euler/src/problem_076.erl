%% @author std-string

%% It is possible to write five as a sum in exactly six different ways:
%% 4 + 1
%% 3 + 2
%% 3 + 1 + 1
%% 2 + 2 + 1
%% 2 + 1 + 1 + 1
%% 1 + 1 + 1 + 1 + 1
%% How many different ways can one hundred be written as a sum of at least two positive integers?

-module(problem_076).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() -> [{5, 6}, {100, 190569291}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(Number) ->
    AvailableNumbers = lists:seq(1, Number - 1),
    WayStorage = calc_ways(Number, AvailableNumbers),
    array:get(Number, WayStorage).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO (std_string) : move into common libs
calc_ways(MaxNumber, [FirstItem | _] = Items) ->
    WayStorage = array:new([{size, MaxNumber + 1}, {fixed, true}, {default, 0}]),
    calc_ways(FirstItem, MaxNumber, Items, array:set(0, 1, WayStorage)).

calc_ways(Number, MaxNumber, [_HeadItem | ItemsRest], WayStorage) when Number > MaxNumber ->
    case ItemsRest of
        [] -> WayStorage;
        [NextHeadItem | _] -> calc_ways(NextHeadItem, MaxNumber, ItemsRest, WayStorage)
    end;
calc_ways(Number, MaxNumber, [HeadItem | _] = Items, WayStorage) ->
    CurrentValue = array:get(Number, WayStorage),
    PrevValue = array:get(Number - HeadItem, WayStorage),
    calc_ways(Number + 1, MaxNumber, Items, array:set(Number, CurrentValue + PrevValue, WayStorage)).