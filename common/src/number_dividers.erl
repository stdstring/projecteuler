-module(number_dividers).

-export([get_dividers/1, is_prime/1]).

-include("primes_def.hrl").

%% get all dividers for number. interface
-spec get_dividers(Number :: pos_integer()) -> [pos_integer()].
get_dividers(Number) when Number =< 0 -> throw(ebadarg);
get_dividers(1) -> [1];
get_dividers(Number) -> get_dividers(Number, 2, trunc(math:sqrt(Number)), [1], [Number]).

%% get all dividers for number. implementation
-spec get_dividers(Number :: pos_integer(), Current :: pos_integer(), Bound :: pos_integer(), SmallDividers :: [pos_integer()], BigDividers :: [pos_integer()]) -> [pos_integer()].
get_dividers(_, Current, Bound, SmallDividers, BigDividers) when Current > Bound -> lists:reverse(SmallDividers) ++ BigDividers;
get_dividers(Number, Current, Bound, SmallDividers, BigDividers) when Number rem Current == 0 ->
    PairDivider = Number div Current,
    if
        Current == PairDivider -> get_dividers(Number, Current + 1, Bound, [Current] ++ SmallDividers, BigDividers);
        Current /= PairDivider -> get_dividers(Number, Current + 1, Bound, [Current] ++ SmallDividers, [PairDivider] ++ BigDividers)
    end;
get_dividers(Number, Current, Bound, SmallDividers, BigDividers) -> get_dividers(Number, Current + 1, Bound, SmallDividers, BigDividers).

-spec is_prime(Number :: pos_integer()) -> boolean().
is_prime(Number) when Number =< ?KNOWN_PRIME_TOP_BOUND ->
    lists:member(Number, ?KNOWN_PRIMES);
is_prime(Number) when is_integer(Number), Number > 1 ->
    Rem2 = Number rem 2,
    if
        Rem2 == 0 -> false;
        Rem2 /= 0 -> is_prime_impl(Number, 3)
    end.

-spec is_prime_impl(Number :: pos_integer(), Factor :: pos_integer()) -> boolean().
is_prime_impl(Number, Factor) when Factor * Factor > Number -> true;
is_prime_impl(Number, Factor) ->
    Rem = Number rem Factor,
    if
        Rem == 0 -> false;
        Rem /= 0 -> is_prime_impl(Number, Factor + 2)
    end.