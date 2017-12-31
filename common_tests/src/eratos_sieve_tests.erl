%% @author std-string

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

get_sieve_is_prime_test_() ->
    create_is_prime_entries(eratos_sieve:get_sieve(100)).

calc_sieve_is_prime_test_() ->
    create_is_prime_entries(eratos_sieve:calc_sieve(100)).

get_sieve_get_next_prime_test_() ->
    create_get_next_prime_entries(eratos_sieve:get_sieve(30)).

calc_sieve_get_next_prime_test_() ->
    create_get_next_prime_entries(eratos_sieve:calc_sieve(30)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_get_primes_entry(MaxNumber :: pos_integer()) -> tuple().
create_get_primes_entry(MaxNumber) ->
    {lists:flatten(io_lib:format("primes below ~p", [MaxNumber])), ?_assertEqual(eratos_sieve:calc_primes(MaxNumber), eratos_sieve:get_primes(MaxNumber))}.

-spec create_is_prime_entries(Sieve :: eratos_sieve:sieve()) -> [tuple()].
create_is_prime_entries(Sieve) ->
    [{"check that 2 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(2, Sieve))},
     {"check that 3 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(3, Sieve))},
     {"check that 4 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(4, Sieve))},
     {"check that 5 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(5, Sieve))},
     {"check that 6 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(6, Sieve))},
     {"check that 9 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(9, Sieve))},
     {"check that 13 is prime number", ?_assertEqual(true, eratos_sieve:is_prime(13, Sieve))},
     {"check that 15 isn't prime number", ?_assertEqual(false, eratos_sieve:is_prime(15, Sieve))},
     {"check processing of bad arguments", ?_assertError(badarg, eratos_sieve:is_prime(1, Sieve))}].

-spec create_get_next_prime_entries(Sieve :: eratos_sieve:sieve()) -> [tuple()].
create_get_next_prime_entries(Sieve) ->
    [{"next prime number for 2", ?_assertEqual(3, eratos_sieve:get_next_prime(2, Sieve))},
     {"next prime number for 3", ?_assertEqual(5, eratos_sieve:get_next_prime(3, Sieve))},
     {"next prime number for 4", ?_assertEqual(5, eratos_sieve:get_next_prime(4, Sieve))},
     {"next prime number for 5", ?_assertEqual(7, eratos_sieve:get_next_prime(5, Sieve))},
     {"next prime number for 7", ?_assertEqual(11, eratos_sieve:get_next_prime(7, Sieve))},
     {"next prime number for 8", ?_assertEqual(11, eratos_sieve:get_next_prime(8, Sieve))},
     {"next prime number for 9", ?_assertEqual(11, eratos_sieve:get_next_prime(9, Sieve))},
     {"next prime number for 28", ?_assertEqual(29, eratos_sieve:get_next_prime(28, Sieve))},
     {"next prime number for 29", ?_assertEqual(undef, eratos_sieve:get_next_prime(29, Sieve))},
     {"next prime number for 30", ?_assertEqual(undef, eratos_sieve:get_next_prime(30, Sieve))},
     {"next prime number for 100", ?_assertEqual(undef, eratos_sieve:get_next_prime(100, Sieve))}].