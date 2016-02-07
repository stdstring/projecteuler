-module(eratos_sieve_tests).

-include_lib("eunit/include/eunit.hrl").

get_primes_test_() ->
    [{"primes below 10", ?_assertEqual([2, 3, 5, 7], eratos_sieve:get_primes(10))},
     {"primes below 20", ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19], eratos_sieve:get_primes(20))},
     {"primes below 30", ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], eratos_sieve:get_primes(30))}].