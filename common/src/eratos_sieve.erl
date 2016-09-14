%% @author std-string

-module(eratos_sieve).

-export([get_primes/1, calc_primes/1, get_sieve/1, calc_sieve/1, is_prime/2]).

-include("primes_def.hrl").

-type sieve() :: array:array(boolean()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_primes(MaxNumber :: pos_integer()) -> [pos_integer()].
get_primes(MaxNumber) when MaxNumber =< ?KNOWN_PRIME_TOP_BOUND ->
    lists:takewhile(fun(Number) -> Number =< MaxNumber end, ?KNOWN_PRIMES);
get_primes(MaxNumber) -> calc_primes(MaxNumber).

-spec calc_primes(MaxNumber :: pos_integer()) -> [pos_integer()].
calc_primes(MaxNumber) -> calc_primes_impl(calc_sieve_size(MaxNumber)).

-spec get_sieve(MaxNumber :: pos_integer()) -> sieve().
get_sieve(MaxNumber) when MaxNumber =< ?KNOWN_PRIME_TOP_BOUND ->
    SieveSize = calc_sieve_size(MaxNumber),
    Primes = lists:takewhile(fun(Number) -> Number =< MaxNumber end, ?KNOWN_PRIMES),
    InitSieve = array:new([{size, SieveSize}, {fixed, true}, {default, false}]),
    lists:folds(fun(Prime, Sieve) -> array:set(calc_index(Prime), true, Sieve) end, InitSieve, Primes);
get_sieve(MaxNumber) -> calc_sieve(MaxNumber).

-spec calc_sieve(MaxNumber :: pos_integer()) -> sieve().
calc_sieve(MaxNumber) -> create_sieve(calc_sieve_size(MaxNumber)).

%%-spec is_prime(Number :: 2.., Sieve :: sieve()) -> boolean(). - not compiled
-spec is_prime(Number :: pos_integer(), Sieve :: sieve()) -> boolean().
is_prime(Number, _Sieve) when Number < 2 -> error(badarg);
is_prime(2, _Sieve) -> true;
is_prime(Number, _Sieve) when Number rem 2 == 0 -> false;
is_prime(Number, Sieve) when Number rem 2 /= 0 -> array:get(calc_index(Number), Sieve).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec calc_sieve_size(MaxNumber :: pos_integer()) -> pos_integer().
calc_sieve_size(MaxNumber) when MaxNumber rem 2 == 0 -> (MaxNumber div 2) - 1;
calc_sieve_size(MaxNumber) when MaxNumber rem 2 /= 0 -> MaxNumber div 2.

-spec calc_number(Index :: non_neg_integer()) -> pos_integer().
calc_number(Index) -> (Index * 2) + 3.

-spec calc_index(Number :: pos_integer()) -> non_neg_integer().
calc_index(Number) -> (Number - 3) div 2.

-spec calc_primes_impl(SieveSize :: pos_integer()) -> [pos_integer()].
calc_primes_impl(SieveSize) -> create_number_list(create_sieve(SieveSize), 0, SieveSize, [2]).

-spec create_number_list(Sieve :: sieve(), Index :: non_neg_integer(), SieveSize :: pos_integer(), Dest :: [pos_integer()]) -> [pos_integer()].
create_number_list(_Sieve, SieveSize, SieveSize, Dest) -> lists:reverse(Dest);
create_number_list(Sieve, Index, SieveSize, Dest) ->
    case array:get(Index, Sieve) of
        true -> create_number_list(Sieve, Index + 1, SieveSize, [calc_number(Index)] ++ Dest);
        false -> create_number_list(Sieve, Index + 1, SieveSize, Dest)
    end.

-spec create_sieve(SieveSize :: pos_integer()) -> sieve().
create_sieve(SieveSize) -> process_iteration(array:new([{size, SieveSize}, {fixed, true}, {default, true}]), 0, SieveSize).

-spec process_iteration(Sieve :: sieve(), Index :: non_neg_integer() | 'not_found', SieveSize :: pos_integer()) -> sieve().
process_iteration(Sieve, not_found, _SieveSize) -> Sieve;
process_iteration(Sieve, Index, SieveSize) ->
    Number = calc_number(Index),
    NewSieve = erase_multiple(Sieve, Number * Number, 2 * Number, SieveSize),
    process_iteration(NewSieve, find_next_prime(Sieve, Index + 1, SieveSize), SieveSize).

-spec erase_multiple(Sieve :: sieve(), Number :: pos_integer(), Delta :: pos_integer(), SieveSize :: pos_integer()) -> sieve().
erase_multiple(Sieve, Number, _Delta, SieveSize) when Number >= (2 * SieveSize + 3) -> Sieve;
erase_multiple(Sieve, Number, Delta, SieveSize) ->
    erase_multiple(array:set(calc_index(Number), false, Sieve), Number + Delta, Delta, SieveSize).

-spec find_next_prime(Sieve :: sieve(), Index :: non_neg_integer(), SieveSize :: pos_integer()) -> non_neg_integer() | 'not_found'.
find_next_prime(_Sieve, SieveSize, SieveSize) -> not_found;
find_next_prime(Sieve, Index, SieveSize) ->
    case array:get(Index, Sieve) of
        true -> Index;
        false -> find_next_prime(Sieve, Index + 1, SieveSize)
    end.