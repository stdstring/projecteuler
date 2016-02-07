-module(numbers_tests).

-include_lib("eunit/include/eunit.hrl").

power_test_() ->
    [{"2.0^2", ?_assertError(function_clause, numbers:power(2.0, 2))},
     {"2^-1", ?_assertThrow(badarg, numbers:power(2, -1))},
     {"0^0", ?_assertThrow(badarg, numbers:power(0, 0))},
     {"2^0", ?_assertEqual(1, numbers:power(2, 0))},
     {"-2^3", ?_assertEqual(-8, numbers:power(-2, 3))},
     {"2^3", ?_assertEqual(8, numbers:power(2, 3))}].

factorial_test_() ->
    [{"2.0!", ?_assertError(function_clause, numbers:factorial(2.0))},
     {"-1!", ?_assertError(function_clause, numbers:factorial(-1))},
     {"0!", ?_assertEqual(1, numbers:factorial(0))},
     {"1!", ?_assertEqual(1, numbers:factorial(1))},
     {"2!", ?_assertEqual(2, numbers:factorial(2))},
     {"3!", ?_assertEqual(6, numbers:factorial(3))},
     {"4!", ?_assertEqual(24, numbers:factorial(4))},
     {"5!", ?_assertEqual(120, numbers:factorial(5))}].

get_digits_base10_test_() ->
    [{"get digits for 2.0", ?_assertError(function_clause, numbers:get_digits(2.0))},
     {"get digits for 0", ?_assertEqual([0], numbers:get_digits(0))},
     {"get digits for 1", ?_assertEqual([1], numbers:get_digits(1))},
     {"get digits for 7", ?_assertEqual([7], numbers:get_digits(7))},
     {"get digits for 10", ?_assertEqual([1, 0], numbers:get_digits(10))},
     {"get digits for 79", ?_assertEqual([7, 9], numbers:get_digits(79))},
     {"get digits for 231", ?_assertEqual([2, 3, 1], numbers:get_digits(231))},
     {"get digits for 900", ?_assertEqual([9, 0, 0], numbers:get_digits(900))}].

get_digits_base2_test_() ->
    [{"get digits for 2.0", ?_assertError(function_clause, numbers:get_digits(2.0, 2))},
     {"get digits for 0", ?_assertEqual([0], numbers:get_digits(0, 2))},
     {"get digits for 1", ?_assertEqual([1], numbers:get_digits(1, 2))},
     {"get digits for 3", ?_assertEqual([1, 1], numbers:get_digits(3, 2))},
     {"get digits for 5", ?_assertEqual([1, 0, 1], numbers:get_digits(5, 2))}].

get_digits_error_test_() ->
    [{"get digits for base < 2", ?_assertThrow(badarg, numbers:get_digits(0, 1))},
     {"get digits for base > 10", ?_assertThrow(notsup, numbers:get_digits(0, 11))}].

get_number_base10_test_() ->
    [{"get digits for [0]", ?_assertEqual(0, numbers:get_number([0]))},
     {"get digits for [8]", ?_assertEqual(8, numbers:get_number([8]))},
     {"get digits for [1, 0]", ?_assertEqual(10, numbers:get_number([1, 0]))},
     {"get digits for [3, 7]", ?_assertEqual(37, numbers:get_number([3, 7]))},
     {"get digits for [1, 3, 7]", ?_assertEqual(137, numbers:get_number([1, 3, 7]))}].

get_number_base2_test_() ->
    [{"get digits for [0]", ?_assertEqual(0, numbers:get_number([0], 2))},
     {"get digits for [1]", ?_assertEqual(1, numbers:get_number([1], 2))},
     {"get digits for [1, 1]", ?_assertEqual(3, numbers:get_number([1, 1], 2))},
     {"get digits for [1, 0, 1]", ?_assertEqual(5, numbers:get_number([1, 0, 1], 2))}].

get_number_error_test_() ->
    [{"get number for base < 2", ?_assertThrow(badarg, numbers:get_number([0], 1))},
     {"get number for base > 10", ?_assertThrow(notsup, numbers:get_number([0], 11))},
     {"get number for incorrect digit", ?_assertThrow(badarg, numbers:get_number([8], 2))}].