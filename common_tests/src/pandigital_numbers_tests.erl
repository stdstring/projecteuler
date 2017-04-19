%% @author std-string

-module(pandigital_numbers_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

is_pandigital2_test_() ->
    [{"check before 12", ?_assertNot(pandigital_numbers:is_pandigital(10))},
     {"check 19", ?_assertNot(pandigital_numbers:is_pandigital(19))},
     {"check 20", ?_assertNot(pandigital_numbers:is_pandigital(20))},
     {"check after 21", ?_assertNot(pandigital_numbers:is_pandigital(22))},
     {"check 12", ?_assert(pandigital_numbers:is_pandigital(12))},
     {"check 21", ?_assert(pandigital_numbers:is_pandigital(21))}].

is_pandigital3_test_() ->
    [{"check before 123", ?_assertNot(pandigital_numbers:is_pandigital(121))},
     {"check 210", ?_assertNot(pandigital_numbers:is_pandigital(210))},
     {"check 219", ?_assertNot(pandigital_numbers:is_pandigital(219))},
     {"check 211", ?_assertNot(pandigital_numbers:is_pandigital(211))},
     {"check after 321", ?_assertNot(pandigital_numbers:is_pandigital(323))},
     {"check 321", ?_assert(pandigital_numbers:is_pandigital(321))},
     {"check 231", ?_assert(pandigital_numbers:is_pandigital(231))}].

is_pandigital4_test_() ->
    [{"check before 1234", ?_assertNot(pandigital_numbers:is_pandigital(1233))},
     {"check 3210", ?_assertNot(pandigital_numbers:is_pandigital(3210))},
     {"check 3219", ?_assertNot(pandigital_numbers:is_pandigital(3219))},
     {"check 3211", ?_assertNot(pandigital_numbers:is_pandigital(3211))},
     {"check 3121", ?_assertNot(pandigital_numbers:is_pandigital(3121))},
     {"check 3112", ?_assertNot(pandigital_numbers:is_pandigital(3112))},
     {"check 1321", ?_assertNot(pandigital_numbers:is_pandigital(1321))},
     {"check 1312", ?_assertNot(pandigital_numbers:is_pandigital(1312))},
     {"check after 3421", ?_assertNot(pandigital_numbers:is_pandigital(3422))},
     {"check 4321", ?_assert(pandigital_numbers:is_pandigital(4321))},
     {"check 1432", ?_assert(pandigital_numbers:is_pandigital(1432))}].

is_pandigital5_test_() ->
    [{"check before 12345", ?_assertNot(pandigital_numbers:is_pandigital(11111))},
     {"check 43210", ?_assertNot(pandigital_numbers:is_pandigital(43210))},
     {"check 43219", ?_assertNot(pandigital_numbers:is_pandigital(43219))},
     {"check 43211", ?_assertNot(pandigital_numbers:is_pandigital(43211))},
     {"check 43121", ?_assertNot(pandigital_numbers:is_pandigital(43121))},
     {"check after 54321", ?_assertNot(pandigital_numbers:is_pandigital(54444))},
     {"check 54321", ?_assert(pandigital_numbers:is_pandigital(54321))},
     {"check 34512", ?_assert(pandigital_numbers:is_pandigital(34512))}].

is_pandigital6_test_() ->
    [{"check before 123456", ?_assertNot(pandigital_numbers:is_pandigital(123455))},
     {"check 543210", ?_assertNot(pandigital_numbers:is_pandigital(543210))},
     {"check 543219", ?_assertNot(pandigital_numbers:is_pandigital(543219))},
     {"check 543211", ?_assertNot(pandigital_numbers:is_pandigital(543211))},
     {"check after 654321", ?_assertNot(pandigital_numbers:is_pandigital(654333))},
     {"check 654321", ?_assert(pandigital_numbers:is_pandigital(654321))},
     {"check 543216", ?_assert(pandigital_numbers:is_pandigital(543216))}].

is_pandigital7_test_() ->
    [{"check before 1234567", ?_assertNot(pandigital_numbers:is_pandigital(1034567))},
     {"check 6543210", ?_assertNot(pandigital_numbers:is_pandigital(6543210))},
     {"check 6543219", ?_assertNot(pandigital_numbers:is_pandigital(6543219))},
     {"check 6543211", ?_assertNot(pandigital_numbers:is_pandigital(6543211))},
     {"check after 7654321", ?_assertNot(pandigital_numbers:is_pandigital(7654322))},
     {"check 7654321", ?_assert(pandigital_numbers:is_pandigital(7654321))},
     {"check 5743216", ?_assert(pandigital_numbers:is_pandigital(5743216))}].

is_pandigital8_test_() ->
    [{"check before 12345678", ?_assertNot(pandigital_numbers:is_pandigital(12345666))},
     {"check 76543210", ?_assertNot(pandigital_numbers:is_pandigital(76543210))},
     {"check 76543219", ?_assertNot(pandigital_numbers:is_pandigital(76543219))},
     {"check 76543211", ?_assertNot(pandigital_numbers:is_pandigital(76543211))},
     {"check 76543211", ?_assertNot(pandigital_numbers:is_pandigital(76543211))},
     {"check after 87654321", ?_assertNot(pandigital_numbers:is_pandigital(87654322))},
     {"check 87654321", ?_assert(pandigital_numbers:is_pandigital(87654321))},
     {"check 57432816", ?_assert(pandigital_numbers:is_pandigital(57432816))}].

is_pandigital9_test_() ->
    [{"check before 123456789", ?_assertNot(pandigital_numbers:is_pandigital(123456777))},
     {"check 876543210", ?_assertNot(pandigital_numbers:is_pandigital(876543210))},
     {"check 876543211", ?_assertNot(pandigital_numbers:is_pandigital(876543211))},
     {"check 987654321", ?_assertNot(pandigital_numbers:is_pandigital(987654323))},
     {"check 987654321", ?_assert(pandigital_numbers:is_pandigital(987654321))},
     {"check 574328169", ?_assert(pandigital_numbers:is_pandigital(574328169))}].

is_pandigital10_test_() ->
    [{"check before 1023456789", ?_assertNot(pandigital_numbers:is_pandigital(1023456788))},
     {"check 9876543211", ?_assertNot(pandigital_numbers:is_pandigital(9876543211))},
     {"check after 9876543210", ?_assertNot(pandigital_numbers:is_pandigital(9876543211))},
     {"check 9876543210", ?_assert(pandigital_numbers:is_pandigital(9876543210))},
     {"check 5743028169", ?_assert(pandigital_numbers:is_pandigital(5743028169))}].

is_pandigital_other_test_() ->
    [{"check 1", ?_assertNot(pandigital_numbers:is_pandigital(1))},
     {"check 12345678901", ?_assertNot(pandigital_numbers:is_pandigital(12345678901))}].

%% ====================================================================
%% Internal functions
%% ====================================================================