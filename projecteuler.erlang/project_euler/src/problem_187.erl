%% @author std-string

%% A composite is a number containing at least two prime factors. For example, 15 = 3 × 5; 9 = 3 × 3; 12 = 2 × 2 × 3.
%% There are ten composites below thirty containing precisely two, not necessarily distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.
%% How many composite integers, n < 100000000, have precisely two, not necessarily distinct, prime factors?

-module(problem_187).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MIN_PRIME, 2).

-type primes() :: array:array(Prime :: pos_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{30, 10}, {100000000, 17427258}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    PrimeSup = (MaxNumber - 1) div ?MIN_PRIME,
    PrimesList = eratos_sieve:get_primes(PrimeSup),
    Primes = array:from_list(PrimesList),
    process(Primes, 0, array:size(Primes) - 1, MaxNumber, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(Primes :: primes(),
              FromIndex :: non_neg_integer(),
              ToIndex :: non_neg_integer(),
              MaxNumber :: pos_integer(),
              Sum  :: non_neg_integer()) -> pos_integer().
process(_Primes, FromIndex, ToIndex, _MaxNumber, Sum) when FromIndex > ToIndex -> Sum;
process(Primes, FromIndex, ToIndex, MaxNumber, Sum) ->
    Count = ToIndex - FromIndex + 1,
    NewFromIndex = FromIndex + 1,
    NewFromNumber = array:get(NewFromIndex, Primes),
    NewToIndex = find_to_index(Primes, NewFromNumber, ToIndex, MaxNumber),
    process(Primes, NewFromIndex, NewToIndex, MaxNumber, Sum + Count).

-spec find_to_index(Primes :: primes(), FromNumber :: pos_integer(), ToIndex :: non_neg_integer(), MaxNumber :: pos_integer()) -> non_neg_integer().
find_to_index(Primes, FromNumber, ToIndex, MaxNumber) ->
    ToNumber = array:get(ToIndex, Primes),
    if
        ToNumber * FromNumber < MaxNumber -> ToIndex;
        true -> find_to_index(Primes, FromNumber, ToIndex - 1, MaxNumber)
    end.