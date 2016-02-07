%% The following iterative sequence is defined for the set of positive integers:
%% n -> n/2 (n is even)
%% n -> 3n + 1 (n is odd)
%% Using the rule above and starting with 13, we generate the following sequence:
%% 13  40  20  10  5  16  8  4  2  1
%% It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
%% Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
%% Which starting number, under one million, produces the longest chain?
%% NOTE: Once the chain starts the terms are allowed to go above one million.

-module(problem_014).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{1000000, 837799}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) ->
    NumberStorage = process_numbers(MaxNumber),
    {MaxChainLengthNumber, _MaxChainLength} = find_longest_chain(NumberStorage),
    MaxChainLengthNumber.

find_longest_chain(NumberStorage) ->
    FoldlFun = fun(Index, ChainLength, {MaxNumber, MaxLength}) ->
        if
            (ChainLength > MaxLength) -> {Index + 2, ChainLength};
            true -> {MaxNumber, MaxLength}
        end
    end,
    array:foldl(FoldlFun, {1, 1}, NumberStorage).

process_numbers(MaxNumber) -> process_numbers(MaxNumber, 2, array:new([{size, MaxNumber - 1}, {fixed, true}, {default, 0}])).

process_numbers(MaxNumber, MaxNumber, NumberStorage) ->
    process_number(MaxNumber, NumberStorage);
process_numbers(MaxNumber, CurrentNumber, NumberStorage) ->
    process_numbers(MaxNumber, CurrentNumber + 1, process_number(CurrentNumber, NumberStorage)).

process_number(Number, NumberStorage) -> process_number(Number, Number, 0, NumberStorage).

process_number(Source, 1, StepCount, NumberStorage) ->
    array:set(Source - 2, StepCount, NumberStorage);
process_number(Source, Source, StepCount, _NumberStorage) when StepCount > 1 -> error(infinite_loop);
process_number(Source, Current, StepCount, NumberStorage) ->
    case Source > Current of
        true ->
            Count = array:get(Current - 2, NumberStorage),
            array:set(Source - 2, Count + StepCount, NumberStorage);
        false -> process_number(Source, next_step(Current), StepCount + 1, NumberStorage)
    end.

next_step(N) when N rem 2 == 0 -> N div 2;
next_step(N) when N rem 2 == 1 -> 3 * N + 1.