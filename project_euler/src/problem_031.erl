%% @author std-string

%% In England the currency is made up of pound, P, and pence, p, and there are eight coins in general circulation:
%% 1p, 2p, 5p, 10p, 20p, 50p, 1P (100p) and 2P (200p).
%% It is possible to make 2P in the following way: 
%% 1*1P + 1*50p + 2*20p + 1*5p + 1*2p + 3*1p
%% How many different ways can 2P be made using any number of coins?

-module(problem_031).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{{10, [1, 2, 5]}, 10}, {{200, [1, 2, 5, 10, 20, 50, 100, 200]}, 73682}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve({Sum, AvailableCoins}) ->
    SortedCoins = lists:sort(AvailableCoins),
    WayStorage = calc_ways(Sum, SortedCoins),
    array:get(Sum, WayStorage).

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