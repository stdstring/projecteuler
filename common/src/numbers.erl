%% @author std-string

-module(numbers).
-export([power/2,
         factorial/1,
         get_digits/1,
         get_digits/2,
         get_digits_count/1,
         get_digits_count/2,
         get_number/1,
         get_number/2,
         calc_binomial_coeff/2,
         is_perfect_square/1]).

-type digit() :: 0..9.
-type digits() :: [digit()].

-export_type([digit/0, digits/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% X^Y
-spec power(X :: integer(), Y :: integer()) -> integer().
power(X, Y) when not is_integer(X); not is_integer(Y) -> error(badarg);
power(0, 0) -> error(badarg);
power(_X, Y) when Y < 0 -> error(badarg);
power(X, Y) -> power_impl(X, Y, 1).

%% N!
-spec factorial(Number :: integer()) -> integer().
factorial(Number) when not is_integer(Number); Number < 0 -> error(badarg);
factorial(0) -> 1;
factorial(1) -> 1;
factorial(Number) -> factorial_impl(Number, 1).

-spec get_digits(Number :: non_neg_integer()) -> digits().
get_digits(Number) -> get_digits(Number, 10).

-spec get_digits(Number :: non_neg_integer(), Base :: 2..10) -> [non_neg_integer()].
get_digits(Number, _Base) when not is_integer(Number); Number < 0 ->  error(badarg);
get_digits(_Number, Base) when not is_integer(Base); Base < 2; Base > 10 ->  error(badarg);
get_digits(0, _Base) -> [0];
get_digits(Number, Base) -> get_digits_impl(Number, Base, []).

-spec get_digits_count(Number :: pos_integer()) -> pos_integer().
get_digits_count(Number) -> get_digits_count(Number, 10).

-spec get_digits_count(Number :: pos_integer(), Base :: 2..10) -> pos_integer().
get_digits_count(Number, _Base) when not is_integer(Number); Number < 0 ->  error(badarg);
get_digits_count(_Number, Base) when not is_integer(Base); Base < 2; Base > 10 ->  error(badarg);
get_digits_count(0, _Base) -> 1;
get_digits_count(Number, Base) -> get_digits_count_impl(Number, Base, 0).

-spec get_number(Digits :: digits()) -> non_neg_integer().
get_number(Digits) -> get_number(Digits, 10).

-spec get_number(Digits :: [non_neg_integer()], Base :: 2..10) -> non_neg_integer().
get_number(_Digits, Base) when not is_integer(Base); Base < 2; Base > 10 -> error(badarg);
get_number(Digits, Base) -> get_number_impl(Digits, Base, 0).

-spec calc_binomial_coeff(N :: pos_integer(), K :: integer()) -> pos_integer().
calc_binomial_coeff(N, K) when not is_integer(N); not is_integer(K); N < 0 -> error(badarg);
calc_binomial_coeff(N, K) when K > N -> 0;
calc_binomial_coeff(_N, K) when K < 0 -> 0;
calc_binomial_coeff(N, K) ->
    Denominator = factorial(K),
    Numerator = control:for(N - K + 1, N, 1, fun(Number, Product) -> Number * Product end),
    Numerator div Denominator.

%% TODO (std_string) : think about right place of this function
-spec is_perfect_square(Number :: integer()) -> boolean().
is_perfect_square(Number) when not is_integer(Number); Number < 0 -> error(badarg);
is_perfect_square(Number) ->
    SqrtValue = math:sqrt(Number),
    BottomValue = trunc(SqrtValue),
    TopValue = round(SqrtValue),
    (BottomValue * BottomValue == Number) or (TopValue * TopValue == Number).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec power_impl(X :: integer(), Y :: integer(), Product :: integer()) -> integer().
power_impl(_X, 0, _Product) -> 1;
power_impl(X, 1, Product) -> Product * X;
power_impl(X, Y, Product) -> power_impl(X, Y - 1, Product * X).

-spec factorial_impl(Number :: integer(), Product :: integer()) -> integer().
factorial_impl(1, Product) -> Product;
factorial_impl(Number, Product) -> factorial_impl(Number - 1, Product * Number).

-spec get_digits_impl(Number :: pos_integer(), Base :: 2..10, Digits :: [non_neg_integer()]) -> [non_neg_integer()].
get_digits_impl(0, _Base, Digits) -> Digits;
get_digits_impl(Number, Base, Digits) -> get_digits_impl(Number div Base, Base, [Number rem Base] ++ Digits).

-spec get_digits_count_impl(Number :: pos_integer(), Base :: pos_integer(), Count :: non_neg_integer()) -> pos_integer().
get_digits_count_impl(0, _Base, Count) -> Count;
get_digits_count_impl(Number, Base, Count) -> get_digits_count_impl(Number div Base, Base, Count + 1).

-spec get_number_impl(Digits :: [non_neg_integer()], Base :: 2..10, Number :: non_neg_integer()) -> non_neg_integer().
get_number_impl([], _Base, Number) -> Number;
get_number_impl([Digit | _Rest], Base, _Number) when Digit >= Base -> error(badarg);
get_number_impl([Digit | Rest], Base, Number) -> get_number_impl(Rest, Base, Number * Base + Digit).