%% @author std-string

-module(eratos_sieve_tests).

-include_lib("eunit/include/eunit.hrl").

-include("primes_def.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

get_primes_error_test_() ->
    [{"get_primes for -1", ?_assertError(badarg, eratos_sieve:get_primes(-1))},
     {"get_primes for 0", ?_assertError(badarg, eratos_sieve:get_primes(0))},
     {"get_primes for 1", ?_assertError(badarg, eratos_sieve:get_primes(1))},
     {"get_primes for 3.1415926", ?_assertError(badarg, eratos_sieve:get_primes(3.1415926))}].

get_primes_test_() ->
    [create_get_primes_entry(2),
     create_get_primes_entry(10),
     create_get_primes_entry(20),
     create_get_primes_entry(30),
     create_get_primes_entry(?KNOWN_PRIME_TOP_BOUND),
     create_get_primes_entry(2 * ?KNOWN_PRIME_TOP_BOUND)].

calc_primes_error_test_() ->
    [{"calc_primes for -1", ?_assertError(badarg, eratos_sieve:calc_primes(-1))},
     {"calc_primes for 0", ?_assertError(badarg, eratos_sieve:calc_primes(0))},
     {"calc_primes for 1", ?_assertError(badarg, eratos_sieve:calc_primes(1))},
     {"calc_primes for 3.1415926", ?_assertError(badarg, eratos_sieve:calc_primes(3.1415926))}].

calc_primes_test_() ->
    [{"primes below 2", ?_assertEqual([2], eratos_sieve:calc_primes(2))},
     {"primes below 3", ?_assertEqual([2, 3], eratos_sieve:calc_primes(3))},
     {"primes below 10", ?_assertEqual([2, 3, 5, 7], eratos_sieve:calc_primes(10))},
     {"primes below 20", ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19], eratos_sieve:calc_primes(20))},
     {"primes below 30", ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], eratos_sieve:calc_primes(30))}].

get_sieve_error_test_() ->
    [{"get_sieve for -1", ?_assertError(badarg, eratos_sieve:get_sieve(-1))},
     {"get_sieve for 0", ?_assertError(badarg, eratos_sieve:get_sieve(0))},
     {"get_sieve for 1", ?_assertError(badarg, eratos_sieve:get_sieve(1))},
     {"get_sieve for 3.1415926", ?_assertError(badarg, eratos_sieve:get_sieve(3.1415926))}].

get_sieve_test_() ->
    create_check_sieve_entries("get_sieve", fun eratos_sieve:get_sieve/1).

calc_sieve_error_test_() ->
    [{"calc_sieve for -1", ?_assertError(badarg, eratos_sieve:calc_sieve(-1))},
     {"calc_sieve for 0", ?_assertError(badarg, eratos_sieve:calc_sieve(0))},
     {"calc_sieve for 1", ?_assertError(badarg, eratos_sieve:calc_sieve(1))},
     {"calc_sieve for 3.1415926", ?_assertError(badarg, eratos_sieve:calc_sieve(3.1415926))}].

calc_sieve_test_() ->
    create_check_sieve_entries("calc_sieve", fun eratos_sieve:calc_sieve/1).

is_prime_error_test_() ->
    Sieve = eratos_sieve:get_sieve(10),
    [{"is_prime for -1", ?_assertError(badarg, eratos_sieve:is_prime(-1, Sieve))},
     {"is_prime for 0", ?_assertError(badarg, eratos_sieve:is_prime(0, Sieve))},
     {"is_prime for 1", ?_assertError(badarg, eratos_sieve:is_prime(1, Sieve))},
     {"is_prime for 3.1415926", ?_assertError(badarg, eratos_sieve:is_prime(3.1415926, Sieve))},
     {"is_prime for bad sieve", ?_assertError(badarg, eratos_sieve:is_prime(2, none))}].

get_sieve_is_prime_test_() ->
    MaxNumber = 100,
    create_is_prime_entries(eratos_sieve:get_sieve(MaxNumber), MaxNumber).

calc_sieve_is_prime_test_() ->
    MaxNumber = 100,
    create_is_prime_entries(eratos_sieve:calc_sieve(MaxNumber), MaxNumber).

get_next_prime_error_test_() ->
    Sieve = eratos_sieve:get_sieve(10),
    [{"get_next_prime for -1", ?_assertError(badarg, eratos_sieve:get_next_prime(-1, Sieve))},
     {"get_next_prime for 0", ?_assertError(badarg, eratos_sieve:get_next_prime(0, Sieve))},
     {"get_next_prime for 1", ?_assertError(badarg, eratos_sieve:get_next_prime(1, Sieve))},
     {"get_next_prime for 3.1415926", ?_assertError(badarg, eratos_sieve:get_next_prime(3.1415926, Sieve))},
     {"get_next_prime for bad sieve", ?_assertError(badarg, eratos_sieve:get_next_prime(2, none))}].

get_sieve_get_next_prime_test_() ->
    MaxNumber = 30,
    create_get_next_prime_entries(eratos_sieve:get_sieve(MaxNumber), MaxNumber).

calc_sieve_get_next_prime_test_() ->
    MaxNumber = 30,
    create_get_next_prime_entries(eratos_sieve:calc_sieve(MaxNumber), MaxNumber).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_get_primes_entry(MaxNumber :: pos_integer()) -> tuple().
create_get_primes_entry(MaxNumber) ->
    {lists:flatten(io_lib:format("primes below ~p", [MaxNumber])), ?_assertEqual(eratos_sieve:calc_primes(MaxNumber), eratos_sieve:get_primes(MaxNumber))}.

-spec create_is_prime_entries(Sieve :: eratos_sieve:sieve(), MaxNumber :: pos_integer()) -> [tuple()].
create_is_prime_entries(Sieve, MaxNumber) ->
    [{"check that 2 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(2, Sieve))},
     {"check that 3 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(3, Sieve))},
     {"check that 4 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(4, Sieve))},
     {"check that 5 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(5, Sieve))},
     {"check that 6 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(6, Sieve))},
     {"check that 9 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(9, Sieve))},
     {"check that 13 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(13, Sieve))},
     {"check that 15 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(15, Sieve))},
     {format("check that result for ~p is undefined", [MaxNumber + 1]), ?_assertEqual(undef, eratos_sieve:is_prime(MaxNumber + 1, Sieve))},
     {format("check that result for ~p is undefined", [2 * MaxNumber]), ?_assertEqual(undef, eratos_sieve:is_prime(2 * MaxNumber, Sieve))}].

-spec create_get_next_prime_entries(Sieve :: eratos_sieve:sieve(), MaxNumber :: pos_integer()) -> [tuple()].
create_get_next_prime_entries(Sieve, MaxNumber) ->
    [{"next prime number for 2", ?_assertEqual(3, eratos_sieve:get_next_prime(2, Sieve))},
     {"next prime number for 3", ?_assertEqual(5, eratos_sieve:get_next_prime(3, Sieve))},
     {"next prime number for 4", ?_assertEqual(5, eratos_sieve:get_next_prime(4, Sieve))},
     {"next prime number for 5", ?_assertEqual(7, eratos_sieve:get_next_prime(5, Sieve))},
     {"next prime number for 7", ?_assertEqual(11, eratos_sieve:get_next_prime(7, Sieve))},
     {"next prime number for 8", ?_assertEqual(11, eratos_sieve:get_next_prime(8, Sieve))},
     {"next prime number for 9", ?_assertEqual(11, eratos_sieve:get_next_prime(9, Sieve))},
     {"next prime number for 10", ?_assertEqual(11, eratos_sieve:get_next_prime(10, Sieve))},
     {"next prime number for 11", ?_assertEqual(13, eratos_sieve:get_next_prime(11, Sieve))},
     {format("next prime number for ~p", [MaxNumber]), ?_assertEqual(undef, eratos_sieve:get_next_prime(MaxNumber, Sieve))},
     {format("next prime number for ~p", [MaxNumber + 1]), ?_assertEqual(undef, eratos_sieve:get_next_prime(MaxNumber + 1, Sieve))},
     {format("next prime number for ~p", [2 * MaxNumber]), ?_assertEqual(undef, eratos_sieve:get_next_prime(2 * MaxNumber, Sieve))}].

-spec create_check_sieve_entries(SieveFunName :: string(),
                                 SieveFun :: fun((MaxNumber :: pos_integer()) -> eratos_sieve:sieve())) -> [tuple()].
create_check_sieve_entries(SieveFunName, SieveFun) ->
    [{format("~s for 2", [SieveFunName]), ?_assert(check_sieve(2, [], SieveFun(2)))},
     {format("~s for 3", [SieveFunName]), ?_assert(check_sieve(3, [1], SieveFun(3)))},
     {format("~s for 4", [SieveFunName]), ?_assert(check_sieve(4, [1], SieveFun(4)))},
     {format("~s for 5", [SieveFunName]), ?_assert(check_sieve(5, [1, 1], SieveFun(5)))},
     {format("~s for 6", [SieveFunName]), ?_assert(check_sieve(6, [1, 1], SieveFun(6)))},
     {format("~s for 7", [SieveFunName]), ?_assert(check_sieve(7, [1, 1, 1], SieveFun(7)))},
     {format("~s for 8", [SieveFunName]), ?_assert(check_sieve(8, [1, 1, 1], SieveFun(8)))},
     {format("~s for 9", [SieveFunName]), ?_assert(check_sieve(9, [1, 1, 1, 0], SieveFun(9)))},
     {format("~s for 11", [SieveFunName]), ?_assert(check_sieve(11, [1, 1, 1, 0, 1], SieveFun(11)))},
     {format("~s for 13", [SieveFunName]), ?_assert(check_sieve(13, [1, 1, 1, 0, 1, 1], SieveFun(13)))},
     {format("~s for 15", [SieveFunName]), ?_assert(check_sieve(15, [1, 1, 1, 0, 1, 1, 0], SieveFun(15)))},
     {format("~s for 17", [SieveFunName]), ?_assert(check_sieve(17, [1, 1, 1, 0, 1, 1, 0, 1], SieveFun(17)))},
     {format("~s for 19", [SieveFunName]), ?_assert(check_sieve(19, [1, 1, 1, 0, 1, 1, 0, 1, 1], SieveFun(19)))},
     {format("~s for 21", [SieveFunName]), ?_assert(check_sieve(21, [1, 1, 1, 0, 1, 1, 0, 1, 1, 0], SieveFun(21)))}].

-spec check_sieve(ExpectedMaxNumber :: pos_integer(),
                  ExpectedData :: [non_neg_integer()], Sieve :: eratos_sieve:sieve()) -> boolean().
check_sieve(2, _ExpectedData, {sieve, 2, none}) -> true;
check_sieve(ExpectedMaxNumber, ExpectedData, {sieve, ActualMaxNumber, SieveData}) ->
    (ExpectedMaxNumber == ActualMaxNumber) and (mutable_uint8_array:to_list(SieveData) == ExpectedData).

%% TODO (std_string) : move into separate module
-spec format(FormatString :: string(), Args :: [term()]) -> string().
format(FormatString, Args) -> lists:flatten(io_lib:format(FormatString, Args)).