-module(eratos_sieve_tests).

-include_lib("eunit/include/eunit.hrl").

-include("primes_def.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

calc_primes_test_() ->
    [{"primes below 10", ?_assertEqual([2, 3, 5, 7], eratos_sieve:calc_primes(10))},
     {"primes below 20", ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19], eratos_sieve:calc_primes(20))},
     {"primes below 30", ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], eratos_sieve:calc_primes(30))}].

get_primes_test_() ->
    [create_get_primes_entry(10),
     create_get_primes_entry(20),
     create_get_primes_entry(30),
     create_get_primes_entry(?KNOWN_PRIME_TOP_BOUND),
     create_get_primes_entry(2 * ?KNOWN_PRIME_TOP_BOUND)].

calc_sieve_test_() ->
    [create_calc_sieve_entry(10, [2, 3, 5, 7]),
     create_calc_sieve_entry(20, [2, 3, 5, 7, 11, 13, 17, 19]),
     create_calc_sieve_entry(30, [2, 3, 5, 7, 11, 13, 17, 19, 23, 29])].

get_sieve_test_() ->
    [create_get_sieve_entry(10),
     create_get_sieve_entry(20),
     create_get_sieve_entry(30),
     create_get_sieve_entry(?KNOWN_PRIME_TOP_BOUND),
     create_get_sieve_entry(2 * ?KNOWN_PRIME_TOP_BOUND)].

is_prime_test_() ->
    Sieve = eratos_sieve:get_sieve(100),
    [{"check that 2 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(2, Sieve))},
     {"check that 3 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(3, Sieve))},
     {"check that 4 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(4, Sieve))},
     {"check that 5 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(5, Sieve))},
     {"check that 6 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(6, Sieve))},
     {"check that 9 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(9, Sieve))},
     {"check that 13 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(13, Sieve))},
     {"check that 15 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(15, Sieve))},
     {"check processing of bad arguments", ?_assertError(badarg, eratos_sieve:is_prime(1, Sieve))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

create_get_primes_entry(MaxNumber) ->
    {lists:flatten(io_lib:format("primes below ~p", [MaxNumber])), ?_assertEqual(eratos_sieve:calc_primes(MaxNumber), eratos_sieve:get_primes(MaxNumber))}.

create_calc_sieve_entry(MaxNumber, [2 | Primes]) ->
    SieveSize = case (MaxNumber rem 2) of
        0 -> (MaxNumber div 2) - 1;
        1 -> MaxNumber div 2
    end,
    SieveTemplate = array:map(fun(_Index, true) -> false end, array:new([{size, SieveSize}, {fixed, true}, {default, true}])),
    ExpectedSieve = lists:foldl(fun(Prime, Sieve) -> array:set((Prime - 3) div 2, true, Sieve) end, SieveTemplate, Primes),
    {lists:flatten(io_lib:format("sieve below ~p", [MaxNumber])), ?_assertEqual(ExpectedSieve, eratos_sieve:calc_sieve(MaxNumber))}.

create_get_sieve_entry(MaxNumber) ->
    {lists:flatten(io_lib:format("sieve below ~p", [MaxNumber])), ?_assertEqual(eratos_sieve:calc_sieve(MaxNumber), eratos_sieve:get_sieve(MaxNumber))}.