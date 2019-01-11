%% @author std-string

%% The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:
%% 1! + 4! + 5! = 1 + 24 + 120 = 145
%% Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169;
%% it turns out that there are only three such loops that exist:
%% 169 -> 363601 -> 1454 -> 169
%% 871 -> 45361 -> 871
%% 872 -> 45362 -> 872
%% It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,
%% 69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
%% 78 -> 45360 -> 871 -> 45361 (-> 871)
%% 540 -> 145 (-> 145)
%% Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.
%% How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?

-module(problem_074).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type factorial_storage() :: array:array(numbers:digit()).
-type chain_storage() :: array:array('undef' | {'index', Index :: non_neg_integer()} | pos_integer()).
-type number_search_result() :: {'true', NextNumber :: pos_integer()} | 'false'.
-type result() :: {'index', CycleStart :: pos_integer(), CycleSize :: pos_integer()} | pos_integer().

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{1000000 - 1, 60}, 402}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxNumber, ExpectedChainSize}) ->
    FactorialStorage = array:from_list(lists:map(fun(Digit) -> numbers:factorial(Digit) end, lists:seq(0, 9))),
    StorageSize = calc_storage_size(MaxNumber),
    InitStorage = array:new([{size, StorageSize}, {fixed, true}, {default, undef}]),
    Storage = process_number(1, MaxNumber, InitStorage, FactorialStorage),
    process_storage(0, MaxNumber, Storage, ExpectedChainSize, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec calc_storage_size(MaxNumber :: pos_integer()) -> pos_integer().
calc_storage_size(MaxNumber) ->
    %% rough solution
    length(numbers:get_digits(MaxNumber)) * numbers:factorial(9).

-spec process_storage(Index :: non_neg_integer(),
                      MaxNumber :: pos_integer(),
                      Storage :: chain_storage(),
                      ExpectedChainSize :: pos_integer(),
                      Count :: pos_integer()) -> pos_integer().
process_storage(MaxNumber, MaxNumber, _Storage, _ExpectedChainSize, Count) -> Count;
process_storage(Index, MaxNumber, Storage, ExpectedChainSize, Count) ->
    case array:get(Index, Storage) of
        ExpectedChainSize -> process_storage(Index + 1, MaxNumber, Storage, ExpectedChainSize, Count + 1);
        _Other -> process_storage(Index + 1, MaxNumber, Storage, ExpectedChainSize, Count)
    end.

-spec process_number(Number :: pos_integer(),
                     MaxNumber :: pos_integer(),
                     Storage :: chain_storage(),
                     FactorialStorage :: factorial_storage()) -> chain_storage().
process_number(Number, MaxNumber, Storage, FactorialStorage) ->
    NewStorage = process_chain(Number, [], 0, Storage, FactorialStorage),
    case find_next_number(Number + 1, MaxNumber, Storage) of
        {true, NextNumber} -> process_number(NextNumber, MaxNumber, NewStorage, FactorialStorage);
        false -> NewStorage
    end.

-spec find_next_number(Number :: pos_integer(), MaxNumber :: pos_integer(), Storage :: chain_storage()) -> number_search_result().
find_next_number(Number, MaxNumber, _Storage) when Number > MaxNumber -> false;
find_next_number(Number, MaxNumber, Storage) ->
    case array:get(Number - 1, Storage) of
        undef -> {true, Number};
        _Other -> find_next_number(Number + 1, MaxNumber, Storage)
    end.

-spec process_chain(Number :: pos_integer(),
                    Chain :: [pos_integer()],
                    ChainIndex :: non_neg_integer(),
                    Storage :: chain_storage(),
                    FactorialStorage :: factorial_storage()) -> chain_storage().
process_chain(Number, Chain, ChainIndex, Storage, FactorialStorage) ->
    case array:get(Number - 1, Storage) of
        undef ->
            NewStorage = array:set(Number - 1, {index, ChainIndex}, Storage),
            process_chain(process_step(Number, FactorialStorage), [Number] ++ Chain, ChainIndex + 1, NewStorage, FactorialStorage);
        {index, Index} -> collect_result(Chain, {index, Number, ChainIndex - Index}, Storage);
        Count -> collect_result(Chain, Count + 1, Storage)
    end.

-spec collect_result(Chain :: [pos_integer()], Result :: result(), Storage :: chain_storage()) -> chain_storage().
collect_result([], _Result, Storage) -> Storage;
collect_result([CycleStart | ChainRest], {index, CycleStart, CycleSize}, Storage) ->
    collect_result(ChainRest, CycleSize + 1, array:set(CycleStart - 1, CycleSize, Storage));
collect_result([Number | ChainRest], {index, CycleStart, CycleSize}, Storage) ->
    collect_result(ChainRest, {index, CycleStart, CycleSize}, array:set(Number - 1, CycleSize, Storage));
collect_result([Number | ChainRest], Count, Storage) ->
    collect_result(ChainRest, Count + 1, array:set(Number - 1, Count, Storage)).

-spec process_step(Number :: pos_integer(), FactorialStorage :: factorial_storage()) -> pos_integer().
process_step(Number, FactorialStorage) ->
    lists:foldl(fun(Digit, Sum) -> array:get(Digit, FactorialStorage) + Sum end, 0, numbers:get_digits(Number)).