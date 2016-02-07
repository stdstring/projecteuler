-module(numbers).
-export([power/2, factorial/1, get_digits/1, get_digits/2, get_number/1, get_number/2]).

%% X^Y interface
-spec power(X :: integer(), Y :: integer()) -> integer().
power(X, Y) when is_integer(X), is_integer(Y) -> power(X, Y, 1).

%% X^Y implementation
-spec power(X :: integer(), Y :: integer(), Product :: integer()) -> integer().
power(0, 0, _Product) -> throw(badarg);
power(_X, 0, _Product) -> 1;
power(_X, Y, _Product) when Y < 0 -> throw(badarg);
power(X, 1, Product) -> Product * X;
power(X, Y, Product) -> power(X, Y - 1, Product * X).

%% N! interface
-spec factorial(Number :: integer()) -> integer().
factorial(0) -> 1;
factorial(1) -> 1;
factorial(Number) when is_integer(Number), Number > 0 -> factorial(Number, 1).

%% N! implementation
-spec factorial(Number :: integer(), Product :: integer()) -> integer().
factorial(1, Product) -> Product;
factorial(Number, Product) -> factorial(Number - 1, Product * Number).

%% get digits of the number.
-spec get_digits(Number :: non_neg_integer()) -> [0..9].
get_digits(Number) -> get_digits(Number, 10).

-spec get_digits(Number :: non_neg_integer(), Base :: 2..10) -> [non_neg_integer()].
get_digits(_Number, Base) when Base < 2 -> throw(badarg);
get_digits(_Number, Base) when Base > 10 -> throw(notsup);
get_digits(0, _Base) -> [0];
get_digits(Number, Base) when is_integer(Number), Number > 0 ->
    get_digits_impl(Number, Base, []).

-spec get_digits_impl(Number :: pos_integer(), Base :: 2..10, Digits :: [non_neg_integer()]) -> [non_neg_integer()].
get_digits_impl(0, _Base, Digits) -> Digits;
get_digits_impl(Number, Base, Digits) ->
    get_digits_impl(Number div Base, Base, [Number rem Base] ++ Digits).

%% construct number from list of digits.
-spec get_number(Digits :: [0..9]) -> non_neg_integer().
get_number(Digits) -> get_number(Digits, 10).

-spec get_number(Digits :: [non_neg_integer()], Base :: 2..10) -> non_neg_integer().
get_number(_Digits, Base) when Base < 2 -> throw(badarg);
get_number(_Digits, Base) when Base > 10 -> throw(notsup);
get_number(Digits, Base) -> get_number_impl(Digits, Base, 0).

-spec get_number_impl(Digits :: [0..9], Base :: 2..10, Number :: non_neg_integer()) -> non_neg_integer().
get_number_impl([], _Base, Number) -> Number;
get_number_impl([Digit | _Rest], Base, _Number) when Digit >= Base -> throw(badarg);
get_number_impl([Digit | Rest], Base, Number) -> get_number_impl(Rest, Base, Number * Base + Digit).