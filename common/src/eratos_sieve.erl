-module(eratos_sieve).

-export([get_primes/1]).

-include("primes_def.hrl").

-spec get_primes(MaxNumber :: pos_integer()) -> [pos_integer()].
get_primes(MaxNumber) when MaxNumber =< ?KNOWN_PRIME_TOP_BOUND ->
    lists:takewhile(fun(Number) -> Number =< MaxNumber end, ?KNOWN_PRIMES);
get_primes(MaxNumber) when MaxNumber rem 2 == 0 ->
    SieveSize = (MaxNumber div 2) - 1,
    get_primes_impl(SieveSize);
get_primes(MaxNumber) when MaxNumber rem 2 /= 0 ->
    SieveSize = MaxNumber div 2,
    get_primes_impl(SieveSize).

-spec get_primes_impl(SieveSize :: pos_integer()) -> [pos_integer()].
get_primes_impl(SieveSize) ->
    %% Number = (Index * 2) + 3
    Sieve = process_iteration(array:new([{size, SieveSize}, {fixed, true}, {default, true}]), 0, SieveSize),
    create_number_list(Sieve, 0, SieveSize, [2]).

-spec create_number_list(Sieve :: array:array(pos_integer()), Index :: non_neg_integer(), SieveSize :: pos_integer(), Dest :: [pos_integer()]) -> [pos_integer()].
create_number_list(_Sieve, SieveSize, SieveSize, Dest) -> lists:reverse(Dest);
create_number_list(Sieve, Index, SieveSize, Dest) ->
    case array:get(Index, Sieve) of
        true ->
            Number = (Index * 2) + 3,
            create_number_list(Sieve, Index + 1, SieveSize, [Number] ++ Dest);
        false -> create_number_list(Sieve, Index + 1, SieveSize, Dest)
    end.

-spec process_iteration(Sieve :: array:array(pos_integer()), Index :: non_neg_integer() | 'not_found', SieveSize :: pos_integer()) -> array:array(pos_integer()).
process_iteration(Sieve, not_found, _SieveSize) -> Sieve;
process_iteration(Sieve, Index, SieveSize) ->
    Number = (Index * 2) + 3,
    NewSieve = erase_multiple(Sieve, Number * Number, 2 * Number, SieveSize),
    process_iteration(NewSieve, find_next_prime(Sieve, Index + 1, SieveSize), SieveSize).

-spec erase_multiple(Sieve :: array:array(pos_integer()), Number :: pos_integer(), Delta :: pos_integer(), SieveSize :: pos_integer()) -> array:array(pos_integer()).
erase_multiple(Sieve, Number, _Delta, SieveSize) when Number >= (2 * SieveSize + 3) -> Sieve;
erase_multiple(Sieve, Number, Delta, SieveSize) ->
    Index = (Number - 3) div 2,
    erase_multiple(array:set(Index, false, Sieve), Number + Delta, Delta, SieveSize).

-spec find_next_prime(Sieve :: array:array(pos_integer()), Index :: non_neg_integer(), SieveSize :: pos_integer()) -> non_neg_integer() | 'not_found'.
find_next_prime(_Sieve, SieveSize, SieveSize) -> not_found;
find_next_prime(Sieve, Index, SieveSize) ->
    case array:get(Index, Sieve) of
        true -> Index;
        false -> find_next_prime(Sieve, Index + 1, SieveSize)
    end.