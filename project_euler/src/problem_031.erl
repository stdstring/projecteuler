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

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{10, [1, 2, 5]}, 10}, {{200, [1, 2, 5, 10, 20, 50, 100, 200]}, 73682}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({Sum, AvailableCoins}) ->
    SortedCoins = lists:sort(AvailableCoins),
    Storage = partition:create_partition_storage(Sum, SortedCoins),
    partition:get_partition_count(Sum, Storage).

%% ====================================================================
%% Internal functions
%% ====================================================================