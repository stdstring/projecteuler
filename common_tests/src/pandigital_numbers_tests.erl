-module(pandigital_numbers_tests).

-include_lib("eunit/include/eunit.hrl").

is_pandigital2_test_() ->
    [{"check 10", ?_assertNot(pandigital_numbers:is_pandigital([1, 0]))},
     {"check 19", ?_assertNot(pandigital_numbers:is_pandigital([1, 9]))},
     {"check 11", ?_assertNot(pandigital_numbers:is_pandigital([1, 1]))},
     {"check 12", ?_assert(pandigital_numbers:is_pandigital([1, 2]))},
     {"check 21", ?_assert(pandigital_numbers:is_pandigital([2, 1]))}].

is_pandigital3_test_() ->
    [{"check 210", ?_assertNot(pandigital_numbers:is_pandigital([2, 1, 0]))},
     {"check 219", ?_assertNot(pandigital_numbers:is_pandigital([2, 1, 9]))},
     {"check 211", ?_assertNot(pandigital_numbers:is_pandigital([2, 1, 1]))},
     {"check 321", ?_assert(pandigital_numbers:is_pandigital([3, 2, 1]))},
     {"check 231", ?_assert(pandigital_numbers:is_pandigital([2, 3, 1]))}].

is_pandigital4_test_() ->
    [{"check 3210", ?_assertNot(pandigital_numbers:is_pandigital([3, 2, 1, 0]))},
     {"check 3219", ?_assertNot(pandigital_numbers:is_pandigital([3, 2, 1, 9]))},
     {"check 3211", ?_assertNot(pandigital_numbers:is_pandigital([3, 2, 1, 1]))},
     {"check 3121", ?_assertNot(pandigital_numbers:is_pandigital([3, 1, 2, 1]))},
     {"check 3112", ?_assertNot(pandigital_numbers:is_pandigital([3, 1, 1, 2]))},
     {"check 1321", ?_assertNot(pandigital_numbers:is_pandigital([1, 3, 2, 1]))},
     {"check 1312", ?_assertNot(pandigital_numbers:is_pandigital([1, 3, 1, 2]))},
     {"check 1132", ?_assertNot(pandigital_numbers:is_pandigital([1, 1, 3, 2]))},
     {"check 4321", ?_assert(pandigital_numbers:is_pandigital([4, 3, 2, 1]))},
     {"check 1432", ?_assert(pandigital_numbers:is_pandigital([1, 4, 3, 2]))}].

is_pandigital5_test_() ->
    [{"check 43210", ?_assertNot(pandigital_numbers:is_pandigital([4, 3, 2, 1, 0]))},
     {"check 43219", ?_assertNot(pandigital_numbers:is_pandigital([4, 3, 2, 1, 9]))},
     {"check 43211", ?_assertNot(pandigital_numbers:is_pandigital([4, 3, 2, 1, 1]))},
     {"check 43121", ?_assertNot(pandigital_numbers:is_pandigital([4, 3, 1, 2, 1]))},
     {"check 54321", ?_assert(pandigital_numbers:is_pandigital([5, 4, 3, 2, 1]))},
     {"check 34512", ?_assert(pandigital_numbers:is_pandigital([3, 4, 5, 1, 2]))}].

is_pandigital6_test_() ->
    [{"check 543210", ?_assertNot(pandigital_numbers:is_pandigital([5, 4, 3, 2, 1, 0]))},
     {"check 543219", ?_assertNot(pandigital_numbers:is_pandigital([5, 4, 3, 2, 1, 9]))},
     {"check 543211", ?_assertNot(pandigital_numbers:is_pandigital([5, 4, 3, 2, 1, 1]))},
     {"check 654321", ?_assert(pandigital_numbers:is_pandigital([6, 5, 4, 3, 2, 1]))},
     {"check 543216", ?_assert(pandigital_numbers:is_pandigital([5, 4, 3, 2, 1, 6]))}].

is_pandigital7_test_() ->
    [{"check 6543210", ?_assertNot(pandigital_numbers:is_pandigital([6, 5, 4, 3, 2, 1, 0]))},
     {"check 6543219", ?_assertNot(pandigital_numbers:is_pandigital([6, 5, 4, 3, 2, 1, 9]))},
     {"check 6543211", ?_assertNot(pandigital_numbers:is_pandigital([6, 5, 4, 3, 2, 1, 1]))},
     {"check 7654321", ?_assert(pandigital_numbers:is_pandigital([7, 6, 5, 4, 3, 2, 1]))},
     {"check 5743216", ?_assert(pandigital_numbers:is_pandigital([5, 7, 4, 3, 2, 1, 6]))}].

is_pandigital8_test_() ->
    [{"check 76543210", ?_assertNot(pandigital_numbers:is_pandigital([7, 6, 5, 4, 3, 2, 1, 0]))},
     {"check 76543219", ?_assertNot(pandigital_numbers:is_pandigital([7, 6, 5, 4, 3, 2, 1, 9]))},
     {"check 76543211", ?_assertNot(pandigital_numbers:is_pandigital([7, 6, 5, 4, 3, 2, 1, 1]))},
     {"check 87654321", ?_assert(pandigital_numbers:is_pandigital([8, 7, 6, 5, 4, 3, 2, 1]))},
     {"check 57432816", ?_assert(pandigital_numbers:is_pandigital([5, 7, 4, 3, 2, 8, 1, 6]))}].

is_pandigital9_test_() ->
    [{"check 876543210", ?_assertNot(pandigital_numbers:is_pandigital([8, 7, 6, 5, 4, 3, 2, 1, 0]))},
     {"check 876543211", ?_assertNot(pandigital_numbers:is_pandigital([8, 7, 6, 5, 4, 3, 2, 1, 1]))},
     {"check 987654321", ?_assert(pandigital_numbers:is_pandigital([9, 8, 7, 6, 5, 4, 3, 2, 1]))},
     {"check 574328169", ?_assert(pandigital_numbers:is_pandigital([5, 7, 4, 3, 2, 8, 1, 6, 9]))}].

is_pandigital10_test_() ->
    [{"check 9876543211", ?_assertNot(pandigital_numbers:is_pandigital([9, 8, 7, 6, 5, 4, 3, 2, 1, 1]))},
     {"check 9876543210", ?_assert(pandigital_numbers:is_pandigital([9, 8, 7, 6, 5, 4, 3, 2, 1, 0]))},
     {"check 5743028169", ?_assert(pandigital_numbers:is_pandigital([5, 7, 4, 3, 0, 2, 8, 1, 6, 9]))}].

is_pandigital_other_test_() ->
    [{"check 1", ?_assertNot(pandigital_numbers:is_pandigital([1]))},
     {"check 12345678901", ?_assertNot(pandigital_numbers:is_pandigital([1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1]))}].