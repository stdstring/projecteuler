%% @author std-string

-module(number_dividers).

-export([get_dividers/1,
         is_prime/1,
         calc_gcd/2,
         create_dividers/1,
         calc_dividers/2,
         create_prime_dividers/1,
         calc_prime_dividers/2,
         get_number_dividers/2,
         calc_euler_function/2]).

%% TODO (std_string) : think about this
-include("primes_def.hrl").

-type dividers() :: sets:set(pos_integer()).
-type dividers_storage() :: array:array(dividers()).

-export_type ([dividers/0, dividers_storage/0]).

-type create_dividers_for_prime_fun() :: fun((Prime :: pos_integer()) -> dividers()).
-type add_divider_fun() :: fun((Divider :: pos_integer(), Number :: pos_integer(), Dividers :: dividers()) -> dividers()).

%% ====================================================================
%% API functions
%% ====================================================================

%% TODO (std_string) : think about this method
-spec get_dividers(Number :: pos_integer()) -> [pos_integer()].
get_dividers(Number) when Number =< 0 -> error(badarg);
get_dividers(1) -> [1];
get_dividers(Number) -> get_dividers_impl(Number, 2, trunc(math:sqrt(Number)), [1], [Number]).

%% TODO (std_string) : think about this method
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

-spec create_dividers(MaxNumber :: pos_integer()) -> dividers_storage().
create_dividers(MaxNumber) when not is_integer(MaxNumber); MaxNumber < 1 -> error(badarg);
create_dividers(MaxNumber) ->
    Sieve = eratos_sieve:get_sieve(MaxNumber),
    calc_dividers(MaxNumber, Sieve).

-spec calc_dividers(MaxNumber :: pos_integer(), Sieve :: eratos_sieve:sieve()) -> dividers_storage().
calc_dividers(MaxNumber, _Sieve) when not is_integer(MaxNumber); MaxNumber < 1 -> error(badarg);
calc_dividers(MaxNumber, Sieve) ->
    calc_dividers_impl(2, MaxNumber, Sieve, array:new([{size, MaxNumber - 1}, {fixed, true}]), fun(Prime) -> sets:from_list([1, Prime]) end, fun add_divider/3).

-spec create_prime_dividers(MaxNumber :: pos_integer()) -> dividers_storage().
create_prime_dividers(MaxNumber) when not is_integer(MaxNumber); MaxNumber < 1 -> error(badarg);
create_prime_dividers(MaxNumber) ->
    Sieve = eratos_sieve:get_sieve(MaxNumber),
    calc_prime_dividers(MaxNumber, Sieve).

-spec calc_prime_dividers(MaxNumber :: pos_integer(), Sieve :: eratos_sieve:sieve()) -> dividers_storage().
calc_prime_dividers(MaxNumber, _Sieve) when not is_integer(MaxNumber); MaxNumber < 1 -> error(badarg);
calc_prime_dividers(MaxNumber, Sieve) ->
    calc_dividers_impl(2, MaxNumber, Sieve, array:new([{size, MaxNumber - 1}, {fixed, true}]), fun(Prime) -> sets:from_list([Prime]) end, fun add_prime_divider/3).

-spec get_number_dividers(Number :: pos_integer(), Storage :: dividers_storage()) -> dividers() | no_return().
get_number_dividers(Number, _Storage) when not is_integer(Number); Number < 1 -> error(badarg);
get_number_dividers(1, _Storage) -> sets:from_list([1]);
get_number_dividers(Number, Storage) -> array:get(Number - 2, Storage).

-spec calc_euler_function(Number :: pos_integer(), PrimeDividers :: [pos_integer()]) -> pos_integer().
calc_euler_function(Number, PrimeDividers) when not is_integer(Number); Number < 1; not is_list(PrimeDividers) -> error(badarg);
calc_euler_function(1, _PrimeDividers) -> 1;
calc_euler_function(Number, PrimeDividers) ->
    {Numerator, Denominator} = lists:foldl(fun(Prime, {ResultN, ResultD}) -> {(Prime - 1) * ResultN, Prime * ResultD} end, {1, 1}, PrimeDividers),
    Number * Numerator div Denominator.

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

-spec set_number_dividers(Number :: pos_integer(), Dividers :: dividers(), Storage :: dividers_storage()) -> dividers_storage().
set_number_dividers(Number, Dividers, Storage) -> array:set(Number - 2, Dividers, Storage).

-spec calc_dividers_impl(Number :: pos_integer(),
                         MaxNumber :: pos_integer(),
                         Sieve :: eratos_sieve:sieve(),
                         Storage :: dividers_storage(),
                         CreateForPrimeFun :: create_dividers_for_prime_fun(),
                         AddDividerFun :: add_divider_fun()) -> dividers_storage().
calc_dividers_impl(Number, MaxNumber, _Sieve, Storage, _CreateForPrimeFun, _AddDividerFun) when Number > MaxNumber -> Storage;
calc_dividers_impl(Number, MaxNumber, Sieve, Storage, CreateForPrimeFun, AddDividerFun) ->
    Dividers = calc_number_dividers_impl(Number, Sieve, Storage, CreateForPrimeFun, AddDividerFun),
    calc_dividers_impl(Number + 1, MaxNumber, Sieve, set_number_dividers(Number, Dividers, Storage), CreateForPrimeFun, AddDividerFun).

-spec calc_number_dividers_impl(Number :: pos_integer(),
                                Sieve :: eratos_sieve:sieve(),
                                Storage :: dividers_storage(),
                                CreateForPrimeFun :: create_dividers_for_prime_fun(),
                                AddDividerFun :: add_divider_fun()) -> dividers().
calc_number_dividers_impl(2, _Sieve, _Storage, CreateForPrimeFun, _AddDividerFun) -> CreateForPrimeFun(2);
calc_number_dividers_impl(Number, _Sieve, Storage, _CreateForPrimeFun, AddDividerFun) when Number rem 2 == 0 ->
    AddDividerFun(2, Number, get_number_dividers(Number div 2, Storage));
calc_number_dividers_impl(Number, Sieve, Storage, CreateForPrimeFun, AddDividerFun) ->
    case eratos_sieve:is_prime(Number, Sieve) of
        true -> CreateForPrimeFun(Number);
        false ->
            Divider = search_odd_divider(Number, 3),
            AddDividerFun(Divider, Number, get_number_dividers(Number div Divider, Storage))
    end.

-spec search_odd_divider(Number :: pos_integer(), Divider :: pos_integer()) -> pos_integer().
search_odd_divider(Number, Divider) when Divider >= Number -> error(invalid_operation);
search_odd_divider(Number, Divider) ->
    case Number rem Divider of
        0 -> Divider;
        _Other -> search_odd_divider(Number, Divider + 2)
    end.

-spec add_prime_divider(Divider :: pos_integer(), Number :: pos_integer(), Dividers :: dividers()) -> dividers().
add_prime_divider(Divider, _Number, Dividers) -> sets:add_element(Divider, Dividers).

-spec add_divider(Divider :: pos_integer(), Number :: pos_integer(), Dividers :: dividers()) -> dividers().
add_divider(Divider, Number, Dividers) ->
    sets:add_element(Number, sets:fold(fun(Value, Dest) -> sets:add_element(Value * Divider, Dest) end, Dividers, Dividers)).