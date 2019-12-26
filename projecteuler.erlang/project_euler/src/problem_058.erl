%% @author std-string

%% Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
%%
%% 37 36 35 34 33 32 31
%% 38 17 16 15 14 13 30
%% 39 18  5  4  3 12 29
%% 40 19  6  1  2 11 28
%% 41 20  7  8  9 10 27
%% 42 21 22 23 24 25 26
%% 43 44 45 46 47 48 49
%%
%% It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
%% If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
%% If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

-module(problem_058).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type process_result() :: {ProcessedCount :: non_neg_integer(), PrimeCount :: non_neg_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{0.1, 26241}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

%% Notes:
%% Let S(N - 1) - last number from previous layer, L(N) - side size for layer N
%% N1 = S(N - 1) + L(N) - 1, N2 =  N1 + L(N) - 1, N3 =  N2 + L(N) - 1, S(N) = N4 =  N3 + L(N) - 1
-spec solve(PreparedInput :: term()) -> term().
solve(Ratio) -> process_spiral(1, 1, Ratio, {1, 0}).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_spiral(LastNumber :: pos_integer(), SideSize :: pos_integer(), Ratio :: float(), ProcessResult :: process_result()) -> pos_integer().
process_spiral(_LastNumber, SideSize, Ratio, {ProcessedCount, PrimeCount}) when PrimeCount > 0, (PrimeCount / ProcessedCount) < Ratio -> SideSize;
process_spiral(LastNumber, SideSize, Ratio, {ProcessedCount, PrimeCount}) ->
    NewSideSize = SideSize + 2,
    Delta =  NewSideSize - 1,
    N1 = LastNumber + Delta,
    N2 = N1 + Delta,
    N3 = N2 + Delta,
    N4 = N3 + Delta,
    {NewProcessedCount, NewPrimeCount} = process_numbers([N1, N2, N3, N4], {ProcessedCount, PrimeCount}),
    process_spiral(N4, NewSideSize, Ratio, {NewProcessedCount, NewPrimeCount}).

-spec process_numbers(Numbers :: [pos_integer()], ProcessResult :: process_result()) -> process_result().
process_numbers([], {ProcessedCount, PrimeCount}) -> {ProcessedCount, PrimeCount};
process_numbers([Number | Numbers], {ProcessedCount, PrimeCount}) ->
    case number_dividers:is_prime(Number) of
        true -> process_numbers(Numbers, {ProcessedCount + 1, PrimeCount + 1});
        false -> process_numbers(Numbers, {ProcessedCount + 1, PrimeCount})
    end.