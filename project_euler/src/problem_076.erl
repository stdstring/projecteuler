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

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{5, 6}, {100, 190569291}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(Number) ->
    AvailableNumbers = lists:seq(1, Number - 1),
    Storage = partition:create_partition_storage(Number, AvailableNumbers),
    partition:get_partition_count(Number, Storage).

%% ====================================================================
%% Internal functions
%% ====================================================================