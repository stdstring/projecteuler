%% @author std-string

%% A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
%% For example,
%% 44 → 32 → 13 → 10 → 1 → 1
%% 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
%% Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
%% What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
%% How many starting numbers below ten million will arrive at 89?

-module(problem_092).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(STORAGE_MIN_SIZE, 1000).

-type storage_type() :: array:array('happy' | 'unhappy' | 'undef').

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{100, 80}, {1000, 857}, {10000000 - 1, 8581146}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) ->
    StorageSize = calc_storage_size(MaxNumber),
    EmptyStorage = array:new([{size, StorageSize}, {fixed, true}, {default, undef}]),
    SourceStorage = set_known_happy(set_known_unhappy(EmptyStorage)),
    DestStorage = process_number(1, MaxNumber, SourceStorage),
    process_storage(DestStorage, MaxNumber).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec calc_storage_size(MaxNumber :: pos_integer()) -> pos_integer().
calc_storage_size(MaxNumber) when MaxNumber < ?STORAGE_MIN_SIZE -> ?STORAGE_MIN_SIZE;
calc_storage_size(MaxNumber) -> MaxNumber.

-spec set_known_happy(SourceStorage :: storage_type()) -> storage_type().
set_known_happy(SourceStorage) ->
    lists:foldl(fun(Number, Storage) -> array:set(Number - 1, happy, Storage) end, SourceStorage, [1]).

-spec set_known_unhappy(SourceStorage :: storage_type()) -> storage_type().
set_known_unhappy(SourceStorage) ->
    lists:foldl(fun(Number, Storage) -> array:set(Number - 1, unhappy, Storage) end, SourceStorage, [4, 16, 37, 58, 89, 145, 42, 20]).

-spec process_number(Number :: pos_integer(), MaxNumber :: pos_integer(), Storage :: storage_type()) -> storage_type().
process_number(Number, MaxNumber, Storage) ->
    {Result, Chain} = process_chain(Number, [], Storage),
    NewStorage = collect_result(Chain, Result, Storage),
    case find_next_number(Number + 1, MaxNumber, NewStorage) of
        false -> NewStorage;
        {true, NextNumber} -> process_number(NextNumber, MaxNumber, NewStorage)
    end.

-spec find_next_number(Number :: pos_integer(), MaxNumber :: pos_integer(), Storage :: storage_type()) ->
    {'true', NextNumber :: pos_integer()} | 'false'.
find_next_number(Number, MaxNumber, _Storage) when Number > MaxNumber -> false;
find_next_number(Number, MaxNumber, Storage) ->
    case is_known(Number, Storage) of
        false -> {true, Number};
        true -> find_next_number(Number + 1, MaxNumber, Storage)
    end.

-spec process_chain(Number :: pos_integer(), Chain :: [pos_integer()], Storage :: storage_type()) ->
    {Result :: 'happy' | 'unhappy', Chain :: [pos_integer()]}.
process_chain(Number, Chain, Storage) ->
    NextNumber = process_step(Number),
    case is_known(NextNumber, Storage) of
        true -> {array:get(NextNumber - 1, Storage), [Number] ++ Chain};
        false -> process_chain(NextNumber, [Number] ++ Chain, Storage)
    end.

-spec collect_result(Chain :: [pos_integer()], Result :: 'happy' | 'unhappy', Storage :: storage_type()) -> storage_type().
collect_result([], _Result, Storage) ->Storage;
collect_result([Number | Rest], Result, Storage) ->
    collect_result(Rest, Result, array:set(Number - 1, Result, Storage)).

-spec process_step(Number :: pos_integer()) -> pos_integer().
process_step(Number) ->
    lists:foldl(fun(Digit, Sum) -> Digit * Digit + Sum end, 0, numbers:get_digits(Number)).

-spec is_known(Number :: pos_integer(), Storage :: storage_type()) -> boolean().
is_known(Number, Storage) ->
    array:get(Number - 1, Storage) /= undef.

-spec process_storage(Storage :: storage_type(), MaxNumber :: pos_integer()) -> non_neg_integer().
process_storage(Storage, MaxNumber) -> process_storage(Storage, 0, MaxNumber, 0).

-spec process_storage(Storage :: storage_type(), Index :: non_neg_integer(), MaxNumber :: pos_integer(), UnhappyCount :: non_neg_integer()) -> non_neg_integer().
process_storage(_Storage, MaxNumber, MaxNumber, UnhappyCount) -> UnhappyCount;
process_storage(Storage, Index, MaxNumber, UnhappyCount) ->
    case array:get(Index, Storage) of
        unhappy -> process_storage(Storage, Index + 1, MaxNumber, UnhappyCount + 1);
        happy -> process_storage(Storage, Index + 1, MaxNumber, UnhappyCount);
        undef -> error(logic_error)
    end.