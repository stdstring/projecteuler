%% @author std-string

-module(number_dividers).

-export([get_dividers/1, is_prime/1, calc_gcd/2]).

-include("primes_def.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_dividers(Number :: pos_integer()) -> [pos_integer()].
get_dividers(Number) when Number =< 0 -> error(badarg);
get_dividers(1) -> [1];
get_dividers(Number) -> get_dividers_impl(Number, 2, trunc(math:sqrt(Number)), [1], [Number]).

-spec is_prime(Number :: pos_integer()) -> boolean().
is_prime(Number) when Number =< ?KNOWN_PRIME_TOP_BOUND ->
    lists:member(Number, ?KNOWN_PRIMES);
is_prime(Number) when is_integer(Number), Number > 1 ->
    Rem2 = Number rem 2,
    if
        Rem2 == 0 -> false;
        Rem2 /= 0 -> is_prime_impl(Number, 3)
    end.

-spec calc_gcd(A :: integer(), B :: integer()) -> integer().
calc_gcd(A, B) when not is_integer(A); not is_integer(B) -> error(badarg);
calc_gcd(0, 0) -> error(badarg);
calc_gcd(A, B) when A < 0 -> calc_gcd(-A, B);
calc_gcd(A, B) when B < 0 -> calc_gcd(A, -B);
calc_gcd(A, B) when A < B -> calc_gcd(B, A);
calc_gcd(A, A) -> A;
calc_gcd(A, 0) -> A;
calc_gcd(A, B) ->
    Remainder = A rem B,
    case Remainder of
        0 -> B;
        _Other -> calc_gcd(B, Remainder)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_dividers_impl(Number :: pos_integer(),
                        Current :: pos_integer(),
                        Bound :: pos_integer(),
                        SmallDividers :: [pos_integer()],
                        BigDividers :: [pos_integer()]) -> [pos_integer()].
get_dividers_impl(_, Current, Bound, SmallDividers, BigDividers) when Current > Bound -> lists:reverse(SmallDividers) ++ BigDividers;
get_dividers_impl(Number, Current, Bound, SmallDividers, BigDividers) when Number rem Current == 0 ->
    PairDivider = Number div Current,
    if
        Current == PairDivider -> get_dividers_impl(Number, Current + 1, Bound, [Current] ++ SmallDividers, BigDividers);
        Current /= PairDivider -> get_dividers_impl(Number, Current + 1, Bound, [Current] ++ SmallDividers, [PairDivider] ++ BigDividers)
    end;
get_dividers_impl(Number, Current, Bound, SmallDividers, BigDividers) -> get_dividers_impl(Number, Current + 1, Bound, SmallDividers, BigDividers).

-spec is_prime_impl(Number :: pos_integer(), Factor :: pos_integer()) -> boolean().
is_prime_impl(Number, Factor) when Factor * Factor > Number -> true;
is_prime_impl(Number, Factor) ->
    Rem = Number rem Factor,
    if
        Rem == 0 -> false;
        Rem /= 0 -> is_prime_impl(Number, Factor + 2)
    end.