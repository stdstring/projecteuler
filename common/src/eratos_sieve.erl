%% @author std-string

-module(eratos_sieve).

-export([get_primes/1, calc_primes/1, get_sieve/1, calc_sieve/1, is_prime/2, get_next_prime/2]).

-include("primes_def.hrl").

-type sieve() :: mutable_uint8_array:mutable_uint8_array().

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_primes(MaxNumber :: pos_integer()) -> [pos_integer()].
get_primes(MaxNumber) when MaxNumber =< ?KNOWN_PRIME_TOP_BOUND ->
    lists:takewhile(fun(Number) -> Number =< MaxNumber end, ?KNOWN_PRIMES);
get_primes(MaxNumber) -> calc_primes(MaxNumber).

-spec calc_primes(MaxNumber :: pos_integer()) -> [pos_integer()].
calc_primes(MaxNumber) -> create_number_list(calc_sieve(MaxNumber)).

-spec get_sieve(MaxNumber :: pos_integer()) -> sieve().
get_sieve(MaxNumber) when MaxNumber =< ?KNOWN_PRIME_TOP_BOUND ->
    [2 | Primes] = lists:takewhile(fun(Prime) -> Prime =< MaxNumber end, ?KNOWN_PRIMES),
    SieveSize = calc_sieve_size(MaxNumber),
    InitSieve = mutable_uint8_array:create(SieveSize, 0),
    lists:foldl(fun(Prime, Sieve) -> mutable_uint8_array:set(calc_index(Prime), 1, Sieve) end, InitSieve, Primes);
get_sieve(MaxNumber) -> calc_sieve(MaxNumber).

-spec calc_sieve(MaxNumber :: pos_integer()) -> sieve().
calc_sieve(MaxNumber) -> create_sieve(calc_sieve_size(MaxNumber)).

-spec is_prime(Number :: pos_integer(), Sieve :: sieve()) -> boolean().
is_prime(Number, _Sieve) when Number < 2 -> error(badarg);
is_prime(2, _Sieve) -> true;
is_prime(Number, _Sieve) when Number rem 2 == 0 -> false;
is_prime(Number, Sieve) when Number rem 2 /= 0 ->
    case mutable_uint8_array:get(calc_index(Number), Sieve) of
        0 -> false;
        1 -> true
    end.

-spec get_next_prime(Prime :: pos_integer(), Sieve :: sieve()) -> pos_integer() | 'undef'.
get_next_prime(2, _Sieve) -> 3;
get_next_prime(Prime, Sieve) ->
    Index = calc_index(Prime),
    get_next_prime(Index + 1, mutable_uint8_array:size(Sieve), Sieve).

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

-spec create_number_list(Sieve :: sieve()) -> [pos_integer()].
create_number_list(Sieve) -> create_number_list(0, mutable_uint8_array:size(Sieve), Sieve, [2]).

-spec create_number_list(Index :: non_neg_integer(), SieveSize :: pos_integer(), Sieve :: sieve(), Dest :: [pos_integer()]) -> [pos_integer()].
create_number_list(SieveSize, SieveSize, _Sieve, Dest) -> lists:reverse(Dest);
create_number_list(Index, SieveSize, Sieve, Dest) ->
    case mutable_uint8_array:get(Index, Sieve) of
        0 -> create_number_list(Index + 1, SieveSize, Sieve, Dest);
        1 -> create_number_list(Index + 1, SieveSize, Sieve, [calc_number(Index)] ++ Dest)
    end.

-spec create_sieve(SieveSize :: pos_integer()) -> sieve().
create_sieve(SieveSize) ->
    ok = mutable_uint8_array:init(),
    Sieve = mutable_uint8_array:create(SieveSize, 1),
    process_iteration(0, SieveSize, Sieve).

-spec process_iteration(Index :: non_neg_integer() | 'undef', SieveSize :: pos_integer(), Sieve :: sieve()) -> sieve().
process_iteration(undef, _SieveSize, Sieve) -> Sieve;
process_iteration(Index, SieveSize, Sieve) ->
    Number = calc_number(Index),
    NewSieve = erase_multiple(Number * Number, 2 * Number, SieveSize, Sieve),
    process_iteration(find_next_prime_index(Index + 1, SieveSize, Sieve), SieveSize, NewSieve).

-spec erase_multiple(Number :: pos_integer(), Delta :: pos_integer(), SieveSize :: pos_integer(), Sieve :: sieve()) -> sieve().
erase_multiple(Number, _Delta, SieveSize, Sieve) when Number >= (2 * SieveSize + 3) -> Sieve;
erase_multiple(Number, Delta, SieveSize, Sieve) ->
    erase_multiple(Number + Delta, Delta, SieveSize, mutable_uint8_array:set(calc_index(Number), 0, Sieve)).

-spec find_next_prime_index(Index :: non_neg_integer(), SieveSize :: pos_integer(), Sieve :: sieve()) -> non_neg_integer() | 'undef'.
find_next_prime_index(SieveSize, SieveSize, _Sieve) -> undef;
find_next_prime_index(Index, SieveSize, Sieve) ->
    case mutable_uint8_array:get(Index, Sieve) of
        0 -> find_next_prime_index(Index + 1, SieveSize, Sieve);
        1 -> Index
    end.

-spec get_next_prime(Index :: non_neg_integer(), SieveSize :: pos_integer(), Sieve :: sieve()) -> pos_integer() | 'undef'.
get_next_prime(Index, SieveSize, _Sieve) when Index >= SieveSize -> undef;
get_next_prime(Index, SieveSize, Sieve) ->
    case mutable_uint8_array:get(Index, Sieve) of
        0 -> get_next_prime(Index + 1, SieveSize, Sieve);
        1 -> calc_number(Index)
    end.