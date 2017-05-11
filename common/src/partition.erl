%% @author std-string

-module(partition).
-export([create_partition_storage/2, get_partition_count/2]).

-type items() :: [Item :: pos_integer()].
-type partition_storage() :: array:array(PartitionCount :: non_neg_integer()).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% TODO (std_string) : think about working with partially filled storage in future
-spec create_partition_storage(MaxNumber :: pos_integer(), Items :: items()) -> partition_storage().
create_partition_storage(MaxNumber, _Items) when MaxNumber =< 0 -> error(badarg);
create_partition_storage(_MaxNumber, []) -> error(badarg);
create_partition_storage(MaxNumber, Items) ->
    Storage = array:new([{default, 0}]),
    fill_partition_storage(MaxNumber, Items, array:set(0, 1, Storage)).

-spec get_partition_count(Number :: pos_integer(), Storage :: partition_storage()) -> non_neg_integer().
get_partition_count(Number, _Storage) when Number =< 0 -> error(badarg);
get_partition_count(Number, Storage) ->
    Size = array:size(Storage),
    if
        Number < Size -> array:get(Number, Storage);
        Number >= Size -> undef
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

fill_partition_storage(_MaxNumber, [], Storage) -> Storage;
fill_partition_storage(MaxNumber, [HeadItem | ItemsRest], Storage) ->
    fill_partition_storage(MaxNumber, ItemsRest, fill_partition_storage_for_number(HeadItem, MaxNumber, HeadItem, Storage)).

fill_partition_storage_for_number(Number, MaxNumber, _Item, Storage) when Number > MaxNumber -> Storage;
fill_partition_storage_for_number(Number, MaxNumber, Item, Storage) ->
    Value = array:get(Number - Item, Storage),
    CurrentValue = array:get(Number, Storage),
    fill_partition_storage_for_number(Number + 1, MaxNumber, Item, array:set(Number, CurrentValue + Value, Storage)).