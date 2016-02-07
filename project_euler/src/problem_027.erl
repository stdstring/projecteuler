%% Euler discovered the remarkable quadratic formula: n^2 + n + 41
%% It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.
%% However, when n = 40, 40^2 + 40 + 41 = 40 * (40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.
%% The incredible formula  n^2 - 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, -79 and 1601, is -126479.
%% Considering quadratics of the form: n^2 + an + b, where |a| < 1000 and |b| < 1000 where |n| is the modulus/absolute value of n, e.g. |11| = 11 and |-4| = 4
%% Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

-module(problem_027).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{{1000, 1000}, -59231}].

prepare_data(_ModuleSourceDir, Input) -> Input.

%% Notes to solution:
%% 1) N(n = 0) = b > 0 => b > 0 and b is prime number. Easy to show that b > 2 => b is odd number
%% 2) N(n = 1) = 1 + a + b > 0 => a > -(b + 1), a - is odd number (due to b + 1 is even number), a <> 0
%% 3) When n = b, N(n) = b^2 + a*b + b = b * (b  + a + 1) - is not prime number => n in (0, Nmax], Nmax = max(b)^2 + max(a) * max(b) + max(b)
solve({AMax, BMax}) ->
    NMax = calc_value(BMax, AMax, BMax),
    Sieve = eratos_sieve:get_primes(NMax),
    SieveSet = sets:from_list(Sieve),
    [2 | SieveB] = lists:takewhile(fun(Number) -> Number =< BMax end, Sieve),
    {_Number, A, B} = process_coeffb(AMax, SieveB, SieveSet, {0, 0, 0}),
    A * B.

-spec process_coeffb(AMax :: pos_integer(), SieveB :: [pos_integer()], SieveSet :: sets:set(pos_integer()), SavedResult :: {Number :: non_neg_integer(), A :: integer(), B :: integer()}) ->
    {Number :: non_neg_integer(), A :: integer(), B :: integer()}.
process_coeffb(_AMax, [], _SieveSet, SavedResult) -> SavedResult;
process_coeffb(AMax, [B | SieveBRest], SieveSet, SavedResult) ->
    NewSavedResult = process_coeffa(-B, B, AMax, SieveSet, SavedResult),
    process_coeffb(AMax, SieveBRest, SieveSet, NewSavedResult).

-spec process_coeffa(A :: integer(), B :: integer(), AMax :: pos_integer(), SieveSet :: sets:set(pos_integer()), SavedResult :: {Number :: non_neg_integer(), A :: integer(), B :: integer()}) ->
    {Number :: non_neg_integer(), A :: integer(), B :: integer()}.
process_coeffa(A, _B, AMax, _SieveSet, SavedResult) when A > AMax -> SavedResult;
process_coeffa(A, B, AMax, SieveSet, SavedResult) ->
    Result = process_sequence(0, A, B, SieveSet),
    process_coeffa(A + 2, B, AMax, SieveSet, process_result(Result, SavedResult)).

-spec process_sequence(Number :: non_neg_integer(), A :: integer(), B :: integer(), SieveSet :: sets:set(pos_integer())) -> {Number :: non_neg_integer(), A :: integer(), B :: integer()}.
process_sequence(B, A, B, _SieveSet) -> {B - 1, A, B};
process_sequence(Number, A, B, SieveSet) ->
    Value = calc_value(Number, A, B),
    case sets:is_element(Value, SieveSet) of
        true -> process_sequence(Number + 1, A, B, SieveSet);
        false -> {Number - 1, A, B}
    end.

-spec calc_value(Number :: non_neg_integer(), A :: integer(), B :: integer()) -> integer().
calc_value(Number, A, B) -> Number * Number + A * Number + B.

-spec process_result(Result1 :: {Number :: non_neg_integer(), A :: integer(), B :: integer()}, Result2 :: {Number :: non_neg_integer(), A :: integer(), B :: integer()}) ->
    {Number :: non_neg_integer(), A :: integer(), B :: integer()}.
process_result({Number1, A1, B1}, {Number2, A2, B2}) ->
    if
        Number1 >= Number2 -> {Number1, A1, B1};
        Number1 < Number2 -> {Number2, A2, B2}
    end.