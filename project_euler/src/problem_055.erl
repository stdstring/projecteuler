%% @author std-string

%% If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
%% Not all numbers produce palindromes so quickly. For example,
%% 349 + 943 = 1292,
%% 1292 + 2921 = 4213
%% 4213 + 3124 = 7337
%% That is, 349 took three iterations to arrive at a palindrome.
%% Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome.
%% A number that never forms a palindrome through the reverse and add process is called a Lychrel number.
%% Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise.
%% In addition you are given that for every number below ten-thousand,
%% it will either (i) become a palindrome in less than fifty iterations, or,
%% (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome.
%% In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome:
%% 4668731596684224866951378664 (53 iterations, 28-digits).
%% Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
%% How many Lychrel numbers are there below ten-thousand?

-module(problem_055).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MAX_ITERATION, 50).

-type result_storage() :: array:array('true' | 'false' | 'undef').
-type search_result() :: {'true', NextNumber :: pos_integer()} | 'false'.
-type process_chain_result() :: {Result :: boolean(), Chain :: [Number :: pos_integer()]}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{10000, 249}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    Storage = process_number(1, MaxNumber, array:new([{size, MaxNumber}, {fixed, true}, {default, undef}])),
    length(lists:filter(fun(Value) -> Value == false end, array:to_list(Storage))).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_number(Number :: pos_integer(), MaxNumber :: pos_integer(), Storage :: result_storage()) -> result_storage().
process_number(Number, MaxNumber, Storage) ->
    {Result, Chain} = process_chain(Number, [], 0),
    NewStorage = collect_result(Chain, Result, MaxNumber, Storage),
    case find_next_number(Number + 1, NewStorage) of
        false -> NewStorage;
        {true, NextNumber} -> process_number(NextNumber, MaxNumber, NewStorage)
    end.

-spec find_next_number(Number :: pos_integer(), Storage :: result_storage()) -> search_result().
find_next_number(Number, Storage) -> find_next_number(Number - 1, array:size(Storage), Storage).

-spec find_next_number(Index :: non_neg_integer(), Count :: pos_integer(), Storage :: result_storage()) -> search_result().
find_next_number(Count, Count, _Storage) -> false;
find_next_number(Index, Count, Storage) ->
    case array:get(Index, Storage) of
        undef -> {true, Index + 1};
        _Other -> find_next_number(Index + 1, Count, Storage)
    end.

-spec process_chain(Number :: pos_integer(), Chain :: [pos_integer()], Iteration :: non_neg_integer()) -> process_chain_result().
process_chain(_Number, Chain, ?MAX_ITERATION) -> {false, lists:reverse(Chain)};
process_chain(Number, Chain, Iteration) ->
    NextNumber = process_step(Number),
    case is_palindromic(NextNumber) of
        true -> {true, lists:reverse(Chain)};
        false -> process_chain(NextNumber, [Number] ++ Chain, Iteration + 1)
    end.

-spec collect_result(Chain :: [pos_integer()], Result :: boolean(), MaxNumber :: pos_integer(), Storage :: result_storage()) -> result_storage().
collect_result([], _Result, _MaxNumber, Storage) -> Storage;
collect_result([Number | _Rest], _Result, MaxNumber, Storage) when Number > MaxNumber -> Storage;
collect_result([Number | Rest], Result, MaxNumber, Storage) ->
    collect_result(Rest, Result, MaxNumber, array:set(Number - 1, Result, Storage)).

-spec process_step(Number :: pos_integer()) -> pos_integer().
process_step(Number) ->
    ReversedNumber = numbers:get_number(lists:reverse(numbers:get_digits(Number))),
    Number + ReversedNumber.

-spec is_palindromic(Number :: pos_integer()) -> boolean().
is_palindromic(Number) ->
    Digits = numbers:get_digits(Number),
    Digits == lists:reverse(Digits).