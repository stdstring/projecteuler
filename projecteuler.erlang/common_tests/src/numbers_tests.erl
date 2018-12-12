%% @author std-string

-module(numbers_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

power_error_test_() ->
    [{"2.1^2", ?_assertError(badarg, numbers:power(2.1, 2))},
     {"2^2.1", ?_assertError(badarg, numbers:power(2, 2.1))},
     {"2^-1", ?_assertError(badarg, numbers:power(2, -1))},
     {"0^0", ?_assertError(badarg, numbers:power(0, 0))}].

power_test_() ->
    [{"2^0", ?_assertEqual(1, numbers:power(2, 0))},
     {"-2^3", ?_assertEqual(-8, numbers:power(-2, 3))},
     {"2^3", ?_assertEqual(8, numbers:power(2, 3))}].

factorial_error_test_() ->
    [{"2.0!", ?_assertError(badarg, numbers:factorial(2.0))},
     {"-1!", ?_assertError(badarg, numbers:factorial(-1))}].

factorial_test_() ->
    [{"0!", ?_assertEqual(1, numbers:factorial(0))},
     {"1!", ?_assertEqual(1, numbers:factorial(1))},
     {"2!", ?_assertEqual(2, numbers:factorial(2))},
     {"3!", ?_assertEqual(6, numbers:factorial(3))},
     {"4!", ?_assertEqual(24, numbers:factorial(4))},
     {"5!", ?_assertEqual(120, numbers:factorial(5))}].

get_digits_base10_test_() ->
    [{"get digits for 0", ?_assertEqual([0], numbers:get_digits(0))},
     {"get digits for 1", ?_assertEqual([1], numbers:get_digits(1))},
     {"get digits for 7", ?_assertEqual([7], numbers:get_digits(7))},
     {"get digits for 10", ?_assertEqual([1, 0], numbers:get_digits(10))},
     {"get digits for 79", ?_assertEqual([7, 9], numbers:get_digits(79))},
     {"get digits for 231", ?_assertEqual([2, 3, 1], numbers:get_digits(231))},
     {"get digits for 900", ?_assertEqual([9, 0, 0], numbers:get_digits(900))}].

get_digits_base2_test_() ->
    [{"get digits for 0", ?_assertEqual([0], numbers:get_digits(0, 2))},
     {"get digits for 1", ?_assertEqual([1], numbers:get_digits(1, 2))},
     {"get digits for 3", ?_assertEqual([1, 1], numbers:get_digits(3, 2))},
     {"get digits for 5", ?_assertEqual([1, 0, 1], numbers:get_digits(5, 2))}].

get_digits_error_test_() ->
    [{"get digits for 2.0 on the base 10", ?_assertError(badarg, numbers:get_digits(2.0))},
     {"get digits for 2.0 on the base 2", ?_assertError(badarg, numbers:get_digits(2.0, 2))},
     {"get digits for base < 2", ?_assertError(badarg, numbers:get_digits(0, 1))},
     {"get digits for base > 10", ?_assertError(badarg, numbers:get_digits(0, 11))}].

get_digits_count_base10_test_() ->
    [{"get digits count for 0", ?_assertEqual(1, numbers:get_digits_count(0))},
     {"get digits count for 1", ?_assertEqual(1, numbers:get_digits_count(1))},
     {"get digits count for 7", ?_assertEqual(1, numbers:get_digits_count(7))},
     {"get digits count for 10", ?_assertEqual(2, numbers:get_digits_count(10))},
     {"get digits count for 79", ?_assertEqual(2, numbers:get_digits_count(79))},
     {"get digits count for 231", ?_assertEqual(3, numbers:get_digits_count(231))},
     {"get digits count for 1931", ?_assertEqual(4, numbers:get_digits_count(1931))}].

get_digits_count_base2_test_() ->
    [{"get digits count for 0", ?_assertEqual(1, numbers:get_digits_count(0, 2))},
     {"get digits count for 1", ?_assertEqual(1, numbers:get_digits_count(1, 2))},
     {"get digits count for 3", ?_assertEqual(2, numbers:get_digits_count(3, 2))},
     {"get digits count for 5", ?_assertEqual(3, numbers:get_digits_count(5, 2))}].

get_digits_count_error_test_() ->
    [{"get digits count for 2.0 on the base 10", ?_assertError(badarg, numbers:get_digits_count(2.0))},
     {"get digits count for 2.0 on the base 2", ?_assertError(badarg, numbers:get_digits_count(2.0, 2))},
     {"get digits count for base < 2", ?_assertError(badarg, numbers:get_digits_count(0, 1))},
     {"get digits count for base > 10", ?_assertError(badarg, numbers:get_digits_count(0, 11))}].

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
    [{"get number for base < 2", ?_assertError(badarg, numbers:get_number([0], 1))},
     {"get number for base > 10", ?_assertError(badarg, numbers:get_number([0], 11))},
     {"get number for incorrect digit", ?_assertError(badarg, numbers:get_number([8], 2))}].

calc_binomial_coeff_error_test_() ->
    [{"calc_binomial_coeff(2.0, 1)", ?_assertError(badarg, numbers:calc_binomial_coeff(2.0, 1))},
     {"calc_binomial_coeff(2, 1.0)", ?_assertError(badarg, numbers:calc_binomial_coeff(2, 1.0))},
     {"calc_binomial_coeff(-1, 666)", ?_assertError(badarg, numbers:calc_binomial_coeff(-1, 666))}].

calc_binomial_coeff_test_() ->
    [{"calc_binomial_coeff(0, 0)", ?_assertEqual(1, numbers:calc_binomial_coeff(0, 0))},
     {"calc_binomial_coeff(1, 0)", ?_assertEqual(1, numbers:calc_binomial_coeff(1, 0))},
     {"calc_binomial_coeff(1, 1)", ?_assertEqual(1, numbers:calc_binomial_coeff(1, 1))},
     {"calc_binomial_coeff(2, 0)", ?_assertEqual(1, numbers:calc_binomial_coeff(2, 0))},
     {"calc_binomial_coeff(2, 1)", ?_assertEqual(2, numbers:calc_binomial_coeff(2, 1))},
     {"calc_binomial_coeff(2, 2)", ?_assertEqual(1, numbers:calc_binomial_coeff(2, 2))},
     {"calc_binomial_coeff(3, 0)", ?_assertEqual(1, numbers:calc_binomial_coeff(3, 0))},
     {"calc_binomial_coeff(3, 1)", ?_assertEqual(3, numbers:calc_binomial_coeff(3, 1))},
     {"calc_binomial_coeff(3, 2)", ?_assertEqual(3, numbers:calc_binomial_coeff(3, 2))},
     {"calc_binomial_coeff(3, 3)", ?_assertEqual(1, numbers:calc_binomial_coeff(3, 3))},
     {"calc_binomial_coeff(4, 0)", ?_assertEqual(1, numbers:calc_binomial_coeff(4, 0))},
     {"calc_binomial_coeff(4, 1)", ?_assertEqual(4, numbers:calc_binomial_coeff(4, 1))},
     {"calc_binomial_coeff(4, 2)", ?_assertEqual(6, numbers:calc_binomial_coeff(4, 2))},
     {"calc_binomial_coeff(4, 3)", ?_assertEqual(4, numbers:calc_binomial_coeff(4, 3))},
     {"calc_binomial_coeff(4, 4)", ?_assertEqual(1, numbers:calc_binomial_coeff(4, 4))},
     {"calc_binomial_coeff(4, -1)", ?_assertEqual(0, numbers:calc_binomial_coeff(4, -1))},
     {"calc_binomial_coeff(4, 666)", ?_assertEqual(0, numbers:calc_binomial_coeff(4, 666))}].

%% TODO (std_string) : think about right place of this function
is_perfect_square_error_test_() ->
    [{"is_perfect_square(2.0)", ?_assertError(badarg, numbers:is_perfect_square(2.0))},
     {"is_perfect_square(-1)", ?_assertError(badarg, numbers:is_perfect_square(-1))}].

%% TODO (std_string) : think about right place of this function
is_perfect_square_test_() ->
    [{"is_perfect_square(0)", ?_assertEqual(true, numbers:is_perfect_square(0))},
     {"is_perfect_square(1)", ?_assertEqual(true, numbers:is_perfect_square(1))},
     {"is_perfect_square(2)", ?_assertEqual(false, numbers:is_perfect_square(2))},
     {"is_perfect_square(3)", ?_assertEqual(false, numbers:is_perfect_square(3))},
     {"is_perfect_square(4)", ?_assertEqual(true, numbers:is_perfect_square(4))},
     {"is_perfect_square(5)", ?_assertEqual(false, numbers:is_perfect_square(5))},
     {"is_perfect_square(6)", ?_assertEqual(false, numbers:is_perfect_square(6))},
     {"is_perfect_square(7)", ?_assertEqual(false, numbers:is_perfect_square(7))},
     {"is_perfect_square(8)", ?_assertEqual(false, numbers:is_perfect_square(8))},
     {"is_perfect_square(9)", ?_assertEqual(true, numbers:is_perfect_square(9))},
     {"is_perfect_square(10)", ?_assertEqual(false, numbers:is_perfect_square(10))},
     {"is_perfect_square(11)", ?_assertEqual(false, numbers:is_perfect_square(11))},
     {"is_perfect_square(14)", ?_assertEqual(false, numbers:is_perfect_square(14))},
     {"is_perfect_square(15)", ?_assertEqual(false, numbers:is_perfect_square(15))},
     {"is_perfect_square(16)", ?_assertEqual(true, numbers:is_perfect_square(16))},
     {"is_perfect_square(17)", ?_assertEqual(false, numbers:is_perfect_square(17))},
     {"is_perfect_square(18)", ?_assertEqual(false, numbers:is_perfect_square(18))},
     {"is_perfect_square(23)", ?_assertEqual(false, numbers:is_perfect_square(23))},
     {"is_perfect_square(24)", ?_assertEqual(false, numbers:is_perfect_square(24))},
     {"is_perfect_square(25)", ?_assertEqual(true, numbers:is_perfect_square(25))},
     {"is_perfect_square(26)", ?_assertEqual(false, numbers:is_perfect_square(26))},
     {"is_perfect_square(27)", ?_assertEqual(false, numbers:is_perfect_square(27))},
     {"is_perfect_square(34)", ?_assertEqual(false, numbers:is_perfect_square(34))},
     {"is_perfect_square(35)", ?_assertEqual(false, numbers:is_perfect_square(35))},
     {"is_perfect_square(36)", ?_assertEqual(true, numbers:is_perfect_square(36))},
     {"is_perfect_square(37)", ?_assertEqual(false, numbers:is_perfect_square(37))},
     {"is_perfect_square(38)", ?_assertEqual(false, numbers:is_perfect_square(38))}].

%% ====================================================================
%% Internal functions
%% ====================================================================