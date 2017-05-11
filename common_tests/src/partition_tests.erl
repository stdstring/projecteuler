%% @author std-string

-module(partition_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_partition_storage_error_test_() ->
    [{"MaxNumber == 0", ?_assertError(badarg, partition:create_partition_storage(0, [1, 2, 3]))},
     {"MaxNumber < 0", ?_assertError(badarg, partition:create_partition_storage(-1, [1, 2, 3]))},
     {"Items == []", ?_assertError(badarg, partition:create_partition_storage(2, []))}].

create_partition_storage_test_() ->
    [create_partition_storage_entry("partitions on prime numbers from 1 to 10", 10, [2, 3, 5, 7, 11, 13, 17, 19], [0, 1, 1, 1, 2, 2, 3, 3, 4, 5]),
     create_partition_storage_entry("partitions on natual numbers from 1 to 10", 10, lists:seq(1, 12), [1, 2, 3, 5, 7, 11, 15, 22, 30, 42]),
     create_partition_storage_entry("some partitions from 1 to 20", 20, [3, 7, 11, 15, 19, 23], [0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 2, 2, 1, 2, 3, 2, 2])].

get_partition_count_error_test_() ->
    Storage = partition:create_partition_storage(3, [1, 2, 3]),
    [{"MaxNumber == 0", ?_assertError(badarg, partition:get_partition_count(0, Storage))},
     {"MaxNumber < 0", ?_assertError(badarg, partition:get_partition_count(-1, Storage))}].

get_partition_count_test_() ->
    %% Storage = [1, 2, 3, 5, 7, 11, 15, 22, 30, 42]
    Storage = partition:create_partition_storage(10, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
    [{"get partition count for 1", ?_assertEqual(1, partition:get_partition_count(1, Storage))},
     {"get partition count for 2", ?_assertEqual(2, partition:get_partition_count(2, Storage))},
     {"get partition count for 3", ?_assertEqual(3, partition:get_partition_count(3, Storage))},
     {"get partition count for 4", ?_assertEqual(5, partition:get_partition_count(4, Storage))},
     {"get partition count for 5", ?_assertEqual(7, partition:get_partition_count(5, Storage))},
     {"get partition count for 6", ?_assertEqual(11, partition:get_partition_count(6, Storage))},
     {"get partition count for 7", ?_assertEqual(15, partition:get_partition_count(7, Storage))},
     {"get partition count for 8", ?_assertEqual(22, partition:get_partition_count(8, Storage))},
     {"get partition count for 9", ?_assertEqual(30, partition:get_partition_count(9, Storage))},
     {"get partition count for 10", ?_assertEqual(42, partition:get_partition_count(10, Storage))},
     {"get partition count for 11", ?_assertEqual(undef, partition:get_partition_count(11, Storage))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

create_partition_storage_entry(Description, MaxNumber, Items, ExpectedResult) ->
    {Description, ?_assertEqual(ExpectedResult, create_partition_storage(MaxNumber, Items))}.

create_partition_storage(MaxNumber, Items) ->
    Storage = partition:create_partition_storage(MaxNumber, Items),
    lists:map(fun(Number) -> partition:get_partition_count(Number, Storage) end, lists:seq(1, MaxNumber)).