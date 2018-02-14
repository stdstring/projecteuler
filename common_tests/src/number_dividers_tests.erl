%% @author std-string

-module(number_dividers_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DIVIDERS2, [{2, [1, 2]}]).
-define(DIVIDERS3, [{2, [1, 2]}, {3, [1, 3]}]).
-define(DIVIDERS4, [{2, [1, 2]}, {3, [1, 3]}, {4, [1, 2, 4]}]).
-define(DIVIDERS5, [{2, [1, 2]}, {3, [1, 3]}, {4, [1, 2, 4]}, {5, [1, 5]}]).
-define(DIVIDERS6, [{2, [1, 2]}, {3, [1, 3]}, {4, [1, 2, 4]}, {5, [1, 5]}, {6, [1, 2, 3, 6]}]).
-define(DIVIDERS7, [{2, [1, 2]}, {3, [1, 3]}, {4, [1, 2, 4]}, {5, [1, 5]}, {6, [1, 2, 3, 6]}, {7, [1, 7]}]).
-define(DIVIDERS8, [{2, [1, 2]}, {3, [1, 3]}, {4, [1, 2, 4]}, {5, [1, 5]}, {6, [1, 2, 3, 6]}, {7, [1, 7]}, {8, [1, 2, 4, 8]}]).
-define(DIVIDERS9, [{2, [1, 2]}, {3, [1, 3]}, {4, [1, 2, 4]}, {5, [1, 5]}, {6, [1, 2, 3, 6]}, {7, [1, 7]}, {8, [1, 2, 4, 8]}, {9, [1, 3, 9]}]).
-define(DIVIDERS10, [{2, [1, 2]}, {3, [1, 3]}, {4, [1, 2, 4]}, {5, [1, 5]}, {6, [1, 2, 3, 6]}, {7, [1, 7]}, {8, [1, 2, 4, 8]}, {9, [1, 3, 9]}, {10, [1, 2, 5, 10]}]).

-define(PRIME_DIVIDERS2, [{2, [2]}]).
-define(PRIME_DIVIDERS3, [{2, [2]}, {3, [3]}]).
-define(PRIME_DIVIDERS4, [{2, [2]}, {3, [3]}, {4, [2]}]).
-define(PRIME_DIVIDERS5, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}]).
-define(PRIME_DIVIDERS6, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}, {6, [2, 3]}]).
-define(PRIME_DIVIDERS7, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}, {6, [2, 3]}, {7, [7]}]).
-define(PRIME_DIVIDERS8, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}, {6, [2, 3]}, {7, [7]}, {8, [2]}]).
-define(PRIME_DIVIDERS9, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}, {6, [2, 3]}, {7, [7]}, {8, [2]}, {9, [3]}]).
-define(PRIME_DIVIDERS10, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}, {6, [2, 3]}, {7, [7]}, {8, [2]}, {9, [3]}, {10, [2, 5]}]).
-define(PRIME_DIVIDERS11, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}, {6, [2, 3]}, {7, [7]}, {8, [2]}, {9, [3]}, {10, [2, 5]}, {11, [11]}]).
-define(PRIME_DIVIDERS12, [{2, [2]}, {3, [3]}, {4, [2]}, {5, [5]}, {6, [2, 3]}, {7, [7]}, {8, [2]}, {9, [3]}, {10, [2, 5]}, {11, [11]}, {12, [2, 3]}]).

%% ====================================================================
%% Test functions
%% ====================================================================

get_dividers_error_test_() ->
    [{"dividers for -1", ?_assertError(badarg, number_dividers:get_dividers(-1))},
     {"dividers for 0", ?_assertError(badarg, number_dividers:get_dividers(0))}].

get_dividers_test_() ->
    [{"dividers for 1", ?_assertEqual([1], number_dividers:get_dividers(1))},
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

calc_gcd_error_test_() ->
    [{"gcd(4.0, 3)", ?_assertError(badarg, number_dividers:calc_gcd(4.0, 3))},
     {"gcd(4, 3.0)", ?_assertError(badarg, number_dividers:calc_gcd(4, 3.0))},
     {"gcd(0, 0)", ?_assertError(badarg, number_dividers:calc_gcd(0, 0))}].

calc_gcd_test_() ->
    [{"gcd(4, 0)", ?_assertEqual(4, number_dividers:calc_gcd(4, 0))},
     {"gcd(0, 4)", ?_assertEqual(4, number_dividers:calc_gcd(0, 4))},
     {"gcd(4, 4)", ?_assertEqual(4, number_dividers:calc_gcd(4, 4))},
     {"gcd(15, 11)", ?_assertEqual(1, number_dividers:calc_gcd(15, 11))},
     {"gcd(11, 15)", ?_assertEqual(1, number_dividers:calc_gcd(11, 15))},
     {"gcd(15, 12)", ?_assertEqual(3, number_dividers:calc_gcd(15, 12))},
     {"gcd(12, 15)", ?_assertEqual(3, number_dividers:calc_gcd(12, 15))},
     {"gcd(25, -15)", ?_assertEqual(5, number_dividers:calc_gcd(25, -15))},
     {"gcd(-15, 25)", ?_assertEqual(5, number_dividers:calc_gcd(-15, 25))},
     {"gcd(-25, 15)", ?_assertEqual(5, number_dividers:calc_gcd(-25, 15))},
     {"gcd(15, -25)", ?_assertEqual(5, number_dividers:calc_gcd(15, -25))},
     {"gcd(-25, -15)", ?_assertEqual(5, number_dividers:calc_gcd(-25, -15))},
     {"gcd(-15, -25)", ?_assertEqual(5, number_dividers:calc_gcd(-15, -25))}].

create_dividers_error_test_() ->
    [{"create_dividers for -1", ?_assertError(badarg, number_dividers:create_dividers(-1))},
     {"create_dividers for 0", ?_assertError(badarg, number_dividers:create_dividers(0))},
     {"create_dividers for 3.1415926", ?_assertError(badarg, number_dividers:create_dividers(3.1415926))}].

create_dividers_test_() ->
    create_check_dividers_entries("create_dividers", fun number_dividers:create_dividers/1).

calc_dividers_error_test_() ->
    Sieve = eratos_sieve:get_sieve(10),
    [{"calc_dividers for MaxNumber = -1", ?_assertError(badarg, number_dividers:calc_dividers(-1, Sieve))},
     {"calc_dividers for MaxNumber = 0", ?_assertError(badarg, number_dividers:calc_dividers(0, Sieve))},
     {"calc_dividers for MaxNumber = 3.1415926", ?_assertError(badarg, number_dividers:calc_dividers(3.1415926, Sieve))}].

calc_dividers_test_() ->
    Sieve = eratos_sieve:get_sieve(10),
    create_check_dividers_entries("calc_dividers", fun(MaxNumber) -> number_dividers:calc_dividers(MaxNumber, Sieve) end).

create_prime_dividers_error_test_() ->
    [{"create_prime_dividers for -1", ?_assertError(badarg, number_dividers:create_prime_dividers(-1))},
     {"create_prime_dividers for 0", ?_assertError(badarg, number_dividers:create_prime_dividers(0))},
     {"create_prime_dividers for 3.1415926", ?_assertError(badarg, number_dividers:create_prime_dividers(3.1415926))}].

create_prime_dividers_test_() ->
    create_check_prime_dividers_entries("create_prime_dividers", fun number_dividers:create_prime_dividers/1).

calc_prime_dividers_error_test_() ->
    Sieve = eratos_sieve:get_sieve(10),
    [{"calc_prime_dividers for MaxNumber = -1", ?_assertError(badarg, number_dividers:calc_prime_dividers(-1, Sieve))},
     {"calc_prime_dividers for MaxNumber = 0", ?_assertError(badarg, number_dividers:calc_prime_dividers(0, Sieve))},
     {"calc_prime_dividers for MaxNumber = 3.1415926", ?_assertError(badarg, number_dividers:calc_prime_dividers(3.1415926, Sieve))}].

calc_prime_dividers_test_() ->
    Sieve = eratos_sieve:get_sieve(12),
    create_check_prime_dividers_entries("calc_prime_dividers", fun(MaxNumber) -> number_dividers:calc_prime_dividers(MaxNumber, Sieve) end).

get_number_dividers_error_test_() ->
    MaxNumber = 10,
    Storage = number_dividers:create_dividers(MaxNumber),
    [{"get_number_dividers for -1", ?_assertError(badarg, number_dividers:get_number_dividers(-1, Storage))},
     {"get_number_dividers for 0", ?_assertError(badarg, number_dividers:get_number_dividers(0, Storage))},
     {"get_number_dividers for 3.1415926", ?_assertError(badarg, number_dividers:get_number_dividers(3.1415926, Storage))},
     {"get_number_dividers for MaxNumber + 1", ?_assertError(badarg, number_dividers:get_number_dividers(MaxNumber + 1, Storage))}].

get_number_dividers_test_() ->
    Storage = number_dividers:create_dividers(10),
    [{"get_number_dividers for 2", ?_assert(check_dividers([1, 2], number_dividers:get_number_dividers(2, Storage)))},
     {"get_number_dividers for 3", ?_assert(check_dividers([1, 3], number_dividers:get_number_dividers(3, Storage)))},
     {"get_number_dividers for 4", ?_assert(check_dividers([1, 2, 4], number_dividers:get_number_dividers(4, Storage)))},
     {"get_number_dividers for 5", ?_assert(check_dividers([1, 5], number_dividers:get_number_dividers(5, Storage)))},
     {"get_number_dividers for 6", ?_assert(check_dividers([1, 2, 3, 6], number_dividers:get_number_dividers(6, Storage)))},
     {"get_number_dividers for 7", ?_assert(check_dividers([1, 7], number_dividers:get_number_dividers(7, Storage)))},
     {"get_number_dividers for 8", ?_assert(check_dividers([1, 2, 4, 8], number_dividers:get_number_dividers(8, Storage)))},
     {"get_number_dividers for 9", ?_assert(check_dividers([1, 3, 9], number_dividers:get_number_dividers(9, Storage)))},
     {"get_number_dividers for 10", ?_assert(check_dividers([1, 2, 5, 10], number_dividers:get_number_dividers(10, Storage)))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_check_dividers_entries(CreateFunName :: string(),
                                    CreateFun :: fun((MaxNumber :: pos_integer()) -> number_dividers:dividers_storage())) -> [tuple()].
create_check_dividers_entries(CreateFunName, CreateFun) ->
    [{string_utils:format("~s for MaxNumber = 2", [CreateFunName]), ?_assert(check(?DIVIDERS2, CreateFun(2)))},
     {string_utils:format("~s for MaxNumber = 3", [CreateFunName]), ?_assert(check(?DIVIDERS3, CreateFun(3)))},
     {string_utils:format("~s for MaxNumber = 4", [CreateFunName]), ?_assert(check(?DIVIDERS4, CreateFun(4)))},
     {string_utils:format("~s for MaxNumber = 5", [CreateFunName]), ?_assert(check(?DIVIDERS5, CreateFun(5)))},
     {string_utils:format("~s for MaxNumber = 6", [CreateFunName]), ?_assert(check(?DIVIDERS6, CreateFun(6)))},
     {string_utils:format("~s for MaxNumber = 7", [CreateFunName]), ?_assert(check(?DIVIDERS7, CreateFun(7)))},
     {string_utils:format("~s for MaxNumber = 8", [CreateFunName]), ?_assert(check(?DIVIDERS8, CreateFun(8)))},
     {string_utils:format("~s for MaxNumber = 9", [CreateFunName]), ?_assert(check(?DIVIDERS9, CreateFun(9)))},
     {string_utils:format("~s for MaxNumber = 10", [CreateFunName]), ?_assert(check(?DIVIDERS10, CreateFun(10)))}].

-spec create_check_prime_dividers_entries(CreateFunName :: string(),
                                          CreateFun :: fun((MaxNumber :: pos_integer()) -> number_dividers:dividers_storage())) -> [tuple()].
create_check_prime_dividers_entries(CreateFunName, CreateFun) ->
    [{string_utils:format("~s for MaxNumber = 2", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS2, CreateFun(2)))},
     {string_utils:format("~s for MaxNumber = 3", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS3, CreateFun(3)))},
     {string_utils:format("~s for MaxNumber = 4", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS4, CreateFun(4)))},
     {string_utils:format("~s for MaxNumber = 5", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS5, CreateFun(5)))},
     {string_utils:format("~s for MaxNumber = 6", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS6, CreateFun(6)))},
     {string_utils:format("~s for MaxNumber = 7", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS7, CreateFun(7)))},
     {string_utils:format("~s for MaxNumber = 8", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS8, CreateFun(8)))},
     {string_utils:format("~s for MaxNumber = 9", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS9, CreateFun(9)))},
     {string_utils:format("~s for MaxNumber = 10", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS10, CreateFun(10)))},
     {string_utils:format("~s for MaxNumber = 11", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS11, CreateFun(11)))},
     {string_utils:format("~s for MaxNumber = 12", [CreateFunName]), ?_assert(check(?PRIME_DIVIDERS12, CreateFun(12)))}].

-spec check(Expected :: [{Number :: pos_integer(), ExpectedDividers :: [pos_integer()]}],
            Storage :: number_dividers:dividers_storage()) -> 'ok'.
check([], _Storage) -> true;
check([{Number, ExpectedDividers} | Rest], Storage) ->
    ActualDividers = array:get(Number - 2, Storage),
    case check_dividers(ExpectedDividers, ActualDividers) of
        true -> check(Rest, Storage);
        false -> false
    end.

-spec check_dividers(Expected :: [pos_integer()], Actual :: number_dividers:dividers()) -> boolean().
check_dividers(Expected, Actual) ->
    Expected == lists:sort(sets:to_list(Actual)).