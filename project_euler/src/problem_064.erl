%% @author std-string

%% All square roots are periodic when written as continued fractions and can be written in the form:
%% N^(1/2) = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))
%% For example, we can write 23^(1/2) as [4;(1,3,1,8)].
%% Here we use the notation 23^(1/2) = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.
%% The first ten continued fraction representations of (irrational) square roots are:
%% 2^(1/2) = [1;(2)], period = 1
%% 3^(1/2) = [1;(1,2)], period = 2
%% 5^(1/2) = [2;(4)], period = 1
%% 6^(1/2) = [2;(2,4)], period = 2
%% 7^(1/2) = [2;(1,1,1,4)], period = 4
%% 8^(1/2) = [2;(1,4)], period = 2
%% 10^(1/2) = [3;(6)], period = 1
%% 11^(1/2) = [3;(3,6)], period = 2
%% 12^(1/2) = [3;(2,6)], period = 2
%% 13^(1/2) = [3;(1,1,1,1,6)], period = 5
%% Exactly four continued fractions, for N <= 13, have an odd period.
%% How many continued fractions for N <= 10000 have an odd period?

-module(problem_064).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(M0, 0).
-define(D0, 1).
-define(A0_START, 1).
-define(NUMBER_START, 2).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{13, 4}, {10000, 1322}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) -> process_numbers(?NUMBER_START, MaxNumber, ?A0_START, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(Number :: pos_integer(), MaxNumber :: pos_integer(), A0 :: pos_integer(), Count :: non_neg_integer()) -> pos_integer().
process_numbers(Number, MaxNumber, _A0, Count) when Number > MaxNumber -> Count;
process_numbers(Number, MaxNumber, A0, Count) ->
    case process_number(A0, Number) of
        {A0New, []} -> process_numbers(Number + 1, MaxNumber, A0New, Count);
        {A0, AStorage} ->
            StorageLength = length(AStorage),
            Rem = StorageLength rem 2,
            if
                Rem == 0 -> process_numbers(Number + 1, MaxNumber, A0, Count);
                Rem == 1 -> process_numbers(Number + 1, MaxNumber, A0, Count + 1)
            end
    end.

%% TODO (std_string) : probably move to common libs
%% Algorithm (from https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion):
%% N - source number
%% m0 = 0, d0 = 1, a0 = integer(N^(1/2))
%% m = d_prev * a_prev - m_prev
%% d = (N - m * m) / d_prev
%% a = integer((a0 + m) / d)
%% Notice that m, d, and a are always integers. The algorithm terminates when this triplet is the same as one encountered before.
%% The expansion will repeat from then on. The sequence [a0; a1, a2, a3, ...] is the continued fraction expansion: N^(1/2) = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))
-spec process_number(A0 :: pos_integer(), Number :: pos_integer()) -> {A0 :: pos_integer(), AList :: [pos_integer()]}.
process_number(A0, Number) when ((A0 + 1) * (A0 + 1)) == Number -> {(A0 + 1), []};
process_number(A0, Number) -> process_number(A0, Number, {?M0, ?D0, A0}, sets:new(), []).

-spec process_number(A0 :: pos_integer(),
                     Number :: pos_integer(),
                     PrevResult :: {M :: integer(), D :: integer(), A :: integer()},
                     MDStorage :: sets:set({M :: integer(), D :: integer()}),
                     AStorage :: [pos_integer()]) ->
    {A0 :: pos_integer(), AList :: [pos_integer()]}.
process_number(A0, Number, {MPrev, DPrev, APrev}, MDStorage, AStorage) ->
    {M, D, A} = calc_triplet(MPrev, DPrev, APrev, A0, Number),
    case sets:is_element({M, D}, MDStorage) of
        true -> {A0, lists:reverse(AStorage)};
        false -> process_number(A0, Number, {M, D, A}, sets:add_element({M, D}, MDStorage), [A] ++ AStorage)
    end.

-spec calc_triplet(MPrev :: integer(), DPrev :: integer(), APrev :: integer(), A0 :: pos_integer(), Number :: pos_integer()) ->
    {M :: integer(), D :: integer(), A :: integer()}.
calc_triplet(MPrev, DPrev, APrev, A0, Number) ->
    M = DPrev * APrev - MPrev,
    D = (Number - M * M) div DPrev,
    A = (A0 + M) div D,
    {M, D, A}.