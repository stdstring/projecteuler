%% @author std-string

-module(number_dividers_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

get_dividers_test_() ->
    [{"dividers for -1", ?_assertError(badarg, number_dividers:get_dividers(-1))},
     {"dividers for 0", ?_assertError(badarg, number_dividers:get_dividers(0))},
     {"dividers for 1", ?_assertEqual([1], number_dividers:get_dividers(1))},
     {"dividers for 2", ?_assertEqual([1, 2], number_dividers:get_dividers(2))},
     {"dividers for 10", ?_assertEqual([1, 2, 5, 10], number_dividers:get_dividers(10))},
     {"dividers for 19", ?_assertEqual([1, 19], number_dividers:get_dividers(19))},
     {"dividers for 30", ?_assertEqual([1, 2, 3, 5, 6, 10, 15, 30], number_dividers:get_dividers(30))}].

is_prime_test_() ->
    [{"is 2 prime", ?_assert(number_dividers:is_prime(2))},
     {"is 3 prime", ?_assert(number_dividers:is_prime(3))},
     {"is 8 prime", ?_assertNot(number_dividers:is_prime(8))},
     {"is 13 prime", ?_assert(number_dividers:is_prime(13))},
     {"is 49 prime", ?_assertNot(number_dividers:is_prime(49))}].

%% ====================================================================
%% Internal functions
%% ====================================================================