%% @author std-string

-module(pandigital_numbers).
-export([is_pandigital/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%%-spec is_pandigital(Digits :: [0..9]) -> boolean().
%%is_pandigital(Digits) ->
%%    case Digits of
%%        [D1, D2] -> is_pandigital2(D1, D2);
%%        [D1, D2, D3] -> is_pandigital3(D1, D2, D3);
%%        [D1, D2, D3, D4] -> is_pandigital4(D1, D2, D3, D4);
%%        [D1, D2, D3, D4, D5] -> is_pandigital5(D1, D2, D3, D4, D5);
%%        [D1, D2, D3, D4, D5, D6] -> is_pandigital6(D1, D2, D3, D4, D5, D6);
%%        [D1, D2, D3, D4, D5, D6, D7] -> is_pandigital7(D1, D2, D3, D4, D5, D6, D7);
%%        [D1, D2, D3, D4, D5, D6, D7, D8] -> is_pandigital8(D1, D2, D3, D4, D5, D6, D7, D8);
%%        [D1, D2, D3, D4, D5, D6, D7, D8, D9] -> is_pandigital9(D1, D2, D3, D4, D5, D6, D7, D8, D9);
%%        [D1, D2, D3, D4, D5, D6, D7, D8, D9, D10] -> is_pandigital10(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10);
%%        _Other -> false
%%    end.

-spec is_pandigital(Number :: pos_integer()) -> boolean().
is_pandigital(Number) ->
    if
        (12 =< Number) and (Number =< 21) -> is_pandigital_impl(Number, 0, 2#110);
        (123 =< Number) and (Number =< 321) -> is_pandigital_impl(Number, 0, 2#1110);
        (1234 =< Number) and (Number =< 4321) -> is_pandigital_impl(Number, 0, 2#11110);
        (12345 =< Number) and (Number =< 54321) -> is_pandigital_impl(Number, 0, 2#111110);
        (123456 =< Number) and (Number =< 654321) -> is_pandigital_impl(Number, 0, 2#1111110);
        (1234567 =< Number) and (Number =< 7654321) -> is_pandigital_impl(Number, 0, 2#11111110);
        (12345678 =< Number) and (Number =< 87654321) -> is_pandigital_impl(Number, 0, 2#111111110);
        (123456789 =< Number) and (Number =< 987654321) -> is_pandigital_impl(Number, 0, 2#1111111110);
        (1023456789 =< Number) and (Number =< 9876543210) -> is_pandigital_impl(Number, 0, 2#1111111111);
        true -> false
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec is_pandigital_impl(Number :: non_neg_integer(), Result :: non_neg_integer(), ExpectedValue :: pos_integer()) -> boolean().
is_pandigital_impl(0, ExpectedValue, ExpectedValue) -> true;
is_pandigital_impl(0, _Result, _ExpectedValue) -> false;
is_pandigital_impl(Number, Result, ExpectedValue) ->
    is_pandigital_impl(Number div 10, Result bor (1 bsl (Number rem 10)), ExpectedValue).

%%-spec is_pandigital2(D1 :: [0..9], D2 :: [0..9]) -> boolean().
%%is_pandigital2(1, 2) -> true;
%%is_pandigital2(2, 1) -> true;
%%is_pandigital2(_D1, _D2) -> false.

%%-spec is_pandigital3(D1 :: [0..9], D2 :: [0..9], D3 :: [0..9]) -> boolean().
%%is_pandigital3(D, D, _D3) -> false;
%%is_pandigital3(D, _D2, D) -> false;
%%is_pandigital3(_D1, D, D) -> false;
%%is_pandigital3(D1, D2, D3)
%%    when D1 > 0, D1 < 4, D2 > 0, D2 < 4, D3 > 0, D3 < 4 -> true;
%%is_pandigital3(_D1, _D2, _D3) -> false.

%%-spec is_pandigital4(D1 :: [0..9], D2 :: [0..9], D3 :: [0..9], D4 :: [0..9]) -> boolean().
%%is_pandigital4(D, D, _D3, _D4) -> false;
%%is_pandigital4(D, _D2, D, _D4) -> false;
%%is_pandigital4(D, _D2, _D3, D) -> false;
%%is_pandigital4(_D1, D, D, _D4) -> false;
%%is_pandigital4(_D1, D, _D3, D) -> false;
%%is_pandigital4(_D1, _D2, D, D) -> false;
%%is_pandigital4(D1, D2, D3, D4)
%%    when D1 > 0, D1 < 5, D2 > 0, D2 < 5, D3 > 0, D3 < 5, D4 > 0, D4 < 5 -> true;
%%is_pandigital4(_D1, _D2, _D3, _D4) -> false.

%%-spec is_pandigital5(D1 :: [0..9], D2 :: [0..9], D3 :: [0..9], D4 :: [0..9], D5 :: [0..9]) -> boolean().
%%is_pandigital5(D, D, _D3, _D4, _D5) -> false;
%%is_pandigital5(D, _D2, D, _D4, _D5) -> false;
%%is_pandigital5(D, _D2, _D3, D, _D5) -> false;
%%is_pandigital5(D, _D2, _D3, _D4, D) -> false;
%%is_pandigital5(_D1, D, D, _D4, _D5) -> false;
%%is_pandigital5(_D1, D, _D3, D, _D5) -> false;
%%is_pandigital5(_D1, D, _D3, _D4, D) -> false;
%%is_pandigital5(_D1, _D2, D, D, _D5) -> false;
%%is_pandigital5(_D1, _D2, D, _D4, D) -> false;
%%is_pandigital5(_D1, _D2, _D3, D, D) -> false;
%%is_pandigital5(D1, D2, D3, D4, D5)
%%    when D1 > 0, D1 < 6, D2 > 0, D2 < 6, D3 > 0, D3 < 6, D4 > 0, D4 < 6, D5 > 0, D5 < 6 -> true;
%%is_pandigital5(_D1, _D2, _D3, _D4, _D5) -> false.

%%-spec is_pandigital6(D1 :: [0..9], D2 :: [0..9], D3 :: [0..9], D4 :: [0..9], D5 :: [0..9], D6 :: [0..9]) -> boolean().
%%is_pandigital6(D, D, _D3, _D4, _D5, _D6) -> false;
%%is_pandigital6(D, _D2, D, _D4, _D5, _D6) -> false;
%%is_pandigital6(D, _D2, _D3, D, _D5, _D6) -> false;
%%is_pandigital6(D, _D2, _D3, _D4, D, _D6) -> false;
%%is_pandigital6(D, _D2, _D3, _D4, _D5, D) -> false;
%%is_pandigital6(_D1, D, D, _D4, _D5, _D6) -> false;
%%is_pandigital6(_D1, D, _D3, D, _D5, _D6) -> false;
%%is_pandigital6(_D1, D, _D3, _D4, D, _D6) -> false;
%%is_pandigital6(_D1, D, _D3, _D4, _D5, D) -> false;
%%is_pandigital6(_D1, _D2, D, D, _D5, _D6) -> false;
%%is_pandigital6(_D1, _D2, D, _D4, D, _D6) -> false;
%%is_pandigital6(_D1, _D2, D, _D4, _D5, D) -> false;
%%is_pandigital6(_D1, _D2, _D3, D, D, _D6) -> false;
%%is_pandigital6(_D1, _D2, _D3, D, _D5, D) -> false;
%%is_pandigital6(_D1, _D2, _D3, _D4, D, D) -> false;
%%is_pandigital6(D1, D2, D3, D4, D5, D6)
%%    when D1 > 0, D1 < 7, D2 > 0, D2 < 7, D3 > 0, D3 < 7, D4 > 0, D4 < 7, D5 > 0, D5 < 7, D6 > 0, D6 < 7 -> true;
%%is_pandigital6(_D1, _D2, _D3, _D4, _D5, _D6) -> false.

%%-spec is_pandigital7(D1 :: [0..9],
%%                     D2 :: [0..9],
%%                     D3 :: [0..9],
%%                     D4 :: [0..9],
%%                     D5 :: [0..9],
%%                     D6 :: [0..9],
%%                     D7 :: [0..9]) -> boolean().
%%is_pandigital7(D, D, _D3, _D4, _D5, _D6, _D7) -> false;
%%is_pandigital7(D, _D2, D, _D4, _D5, _D6, _D7) -> false;
%%is_pandigital7(D, _D2, _D3, D, _D5, _D6, _D7) -> false;
%%is_pandigital7(D, _D2, _D3, _D4, D, _D6, _D7) -> false;
%%is_pandigital7(D, _D2, _D3, _D4, _D5, D, _D7) -> false;
%%is_pandigital7(D, _D2, _D3, _D4, _D5, _D6, D) -> false;
%%is_pandigital7(_D1, D, D, _D4, _D5, _D6, _D7) -> false;
%%is_pandigital7(_D1, D, _D3, D, _D5, _D6, _D7) -> false;
%%is_pandigital7(_D1, D, _D3, _D4, D, _D6, _D7) -> false;
%%is_pandigital7(_D1, D, _D3, _D4, _D5, D, _D7) -> false;
%%is_pandigital7(_D1, D, _D3, _D4, _D5, _D6, D) -> false;
%%is_pandigital7(_D1, _D2, D, D, _D5, _D6, _D7) -> false;
%%is_pandigital7(_D1, _D2, D, _D4, D, _D6, _D7) -> false;
%%is_pandigital7(_D1, _D2, D, _D4, _D5, D, _D7) -> false;
%%is_pandigital7(_D1, _D2, D, _D4, _D5, _D6, D) -> false;
%%is_pandigital7(_D1, _D2, _D3, D, D, _D6, _D7) -> false;
%%is_pandigital7(_D1, _D2, _D3, D, _D5, D, _D7) -> false;
%%is_pandigital7(_D1, _D2, _D3, D, _D5, _D6, D) -> false;
%%is_pandigital7(_D1, _D2, _D3, _D4, D, D, _D7) -> false;
%%is_pandigital7(_D1, _D2, _D3, _D4, D, _D6, D) -> false;
%%is_pandigital7(_D1, _D2, _D3, _D4, _D5, D, D) -> false;
%%is_pandigital7(D1, D2, D3, D4, D5, D6, D7)
%%    when D1 > 0, D1 < 8, D2 > 0, D2 < 8, D3 > 0, D3 < 8, D4 > 0, D4 < 8, D5 > 0, D5 < 8, D6 > 0, D6 < 8, D7 > 0, D7 < 8 -> true;
%%is_pandigital7(_D1, _D2, _D3, _D4, _D5, _D6, _D7) -> false.

%%-spec is_pandigital8(D1 :: [0..9],
%%                     D2 :: [0..9],
%%                     D3 :: [0..9],
%%                     D4 :: [0..9],
%%                     D5 :: [0..9],
%%                     D6 :: [0..9],
%%                     D7 :: [0..9],
%%                     D8 :: [0..9]) -> boolean().
%%is_pandigital8(D, D, _D3, _D4, _D5, _D6, _D7, _D8) -> false;
%%is_pandigital8(D, _D2, D, _D4, _D5, _D6, _D7, _D8) -> false;
%%is_pandigital8(D, _D2, _D3, D, _D5, _D6, _D7, _D8) -> false;
%%is_pandigital8(D, _D2, _D3, _D4, D, _D6, _D7, _D8) -> false;
%%is_pandigital8(D, _D2, _D3, _D4, _D5, D, _D7, _D8) -> false;
%%is_pandigital8(D, _D2, _D3, _D4, _D5, _D6, D, _D8) -> false;
%%is_pandigital8(D, _D2, _D3, _D4, _D5, _D6, _D7, D) -> false;
%%is_pandigital8(_D1, D, D, _D4, _D5, _D6, _D7, _D8) -> false;
%%is_pandigital8(_D1, D, _D3, D, _D5, _D6, _D7, _D8) -> false;
%%is_pandigital8(_D1, D, _D3, _D4, D, _D6, _D7, _D8) -> false;
%%is_pandigital8(_D1, D, _D3, _D4, _D5, D, _D7, _D8) -> false;
%%is_pandigital8(_D1, D, _D3, _D4, _D5, _D6, D, _D8) -> false;
%%is_pandigital8(_D1, D, _D3, _D4, _D5, _D6, _D7, D) -> false;
%%is_pandigital8(_D1, _D2, D, D, _D5, _D6, _D7, _D8) -> false;
%%is_pandigital8(_D1, _D2, D, _D4, D, _D6, _D7, _D8) -> false;
%%is_pandigital8(_D1, _D2, D, _D4, _D5, D, _D7, _D8) -> false;
%%is_pandigital8(_D1, _D2, D, _D4, _D5, _D6, D, _D8) -> false;
%%is_pandigital8(_D1, _D2, D, _D4, _D5, _D6, _D7, D) -> false;
%%is_pandigital8(_D1, _D2, _D3, D, D, _D6, _D7, _D8) -> false;
%%is_pandigital8(_D1, _D2, _D3, D, _D5, D, _D7, _D8) -> false;
%%is_pandigital8(_D1, _D2, _D3, D, _D5, _D6, D, _D8) -> false;
%%is_pandigital8(_D1, _D2, _D3, D, _D5, _D6, _D7, D) -> false;
%%is_pandigital8(_D1, _D2, _D3, _D4, D, D, _D7, _D8) -> false;
%%is_pandigital8(_D1, _D2, _D3, _D4, D, _D6, D, _D8) -> false;
%%is_pandigital8(_D1, _D2, _D3, _D4, D, _D6, _D7, D) -> false;
%%is_pandigital8(_D1, _D2, _D3, _D4, _D5, D, D, _D8) -> false;
%%is_pandigital8(_D1, _D2, _D3, _D4, _D5, D, _D7, D) -> false;
%%is_pandigital8(_D1, _D2, _D3, _D4, _D5, _D6, D, D) -> false;
%%is_pandigital8(D1, D2, D3, D4, D5, D6, D7, D8)
%%    when D1 > 0, D1 < 9, D2 > 0, D2 < 9, D3 > 0, D3 < 9, D4 > 0, D4 < 9, D5 > 0, D5 < 9, D6 > 0, D6 < 9, D7 > 0, D7 < 9, D8 > 0, D8 < 9 -> true;
%%is_pandigital8(_D1, _D2, _D3, _D4, _D5, _D6, _D7, _D8) -> false.

%%-spec is_pandigital9(D1 :: [0..9],
%%                     D2 :: [0..9],
%%                     D3 :: [0..9],
%%                     D4 :: [0..9],
%%                     D5 :: [0..9],
%%                     D6 :: [0..9],
%%                     D7 :: [0..9],
%%                     D8 :: [0..9],
%%                     D9 :: [0..9]) -> boolean().
%%is_pandigital9(D, D, _D3, _D4, _D5, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(D, _D2, D, _D4, _D5, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(D, _D2, _D3, D, _D5, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(D, _D2, _D3, _D4, D, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(D, _D2, _D3, _D4, _D5, D, _D7, _D8, _D9) -> false;
%%is_pandigital9(D, _D2, _D3, _D4, _D5, _D6, D, _D8, _D9) -> false;
%%is_pandigital9(D, _D2, _D3, _D4, _D5, _D6, _D7, D, _D9) -> false;
%%is_pandigital9(D, _D2, _D3, _D4, _D5, _D6, _D7, _D8, D) -> false;
%%is_pandigital9(_D1, D, D, _D4, _D5, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, D, _D3, D, _D5, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, D, _D3, _D4, D, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, D, _D3, _D4, _D5, D, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, D, _D3, _D4, _D5, _D6, D, _D8, _D9) -> false;
%%is_pandigital9(_D1, D, _D3, _D4, _D5, _D6, _D7, D, _D9) -> false;
%%is_pandigital9(_D1, D, _D3, _D4, _D5, _D6, _D7, _D8, D) -> false;
%%is_pandigital9(_D1, _D2, D, D, _D5, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, D, _D4, D, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, D, _D4, _D5, D, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, D, _D4, _D5, _D6, D, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, D, _D4, _D5, _D6, _D7, D, _D9) -> false;
%%is_pandigital9(_D1, _D2, D, _D4, _D5, _D6, _D7, _D8, D) -> false;
%%is_pandigital9(_D1, _D2, _D3, D, D, _D6, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, D, _D5, D, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, D, _D5, _D6, D, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, D, _D5, _D6, _D7, D, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, D, _D5, _D6, _D7, _D8, D) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, D, D, _D7, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, D, _D6, D, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, D, _D6, _D7, D, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, D, _D6, _D7, _D8, D) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, _D5, D, D, _D8, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, _D5, D, _D7, D, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, _D5, D, _D7, _D8, D) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, _D5, _D6, D, D, _D9) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, _D5, _D6, D, _D8, D) -> false;
%%is_pandigital9(_D1, _D2, _D3, _D4, _D5, _D6, _D7, D, D) -> false;
%%is_pandigital9(D1, D2, D3, D4, D5, D6, D7, D8, D9)
%%    when D1 > 0, D1 =< 9, D2 > 0, D2 =< 9, D3 > 0, D3 =< 9, D4 > 0, D4 =< 9, D5 > 0, D5 =< 9, D6 > 0, D6 =< 9, D7 > 0, D7 =< 9, D8 > 0, D8 =< 9, D9 > 0, D9 =< 9 -> true;
%%is_pandigital9(_D1, _D2, _D3, _D4, _D5, _D6, _D7, _D8, _D9) -> false.

%%-spec is_pandigital10(D1 :: [0..9],
%%                      D2 :: [0..9],
%%                      D3 :: [0..9],
%%                      D4 :: [0..9],
%%                      D5 :: [0..9],
%%                      D6 :: [0..9],
%%                      D7 :: [0..9],
%%                      D8 :: [0..9],
%%                      D9 :: [0..9],
%%                      D10 :: [0..9]) -> boolean().
%%is_pandigital10(D, D, _D3, _D4, _D5, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(D, _D2, D, _D4, _D5, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(D, _D2, _D3, D, _D5, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(D, _D2, _D3, _D4, D, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(D, _D2, _D3, _D4, _D5, D, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(D, _D2, _D3, _D4, _D5, _D6, D, _D8, _D9, _D10) -> false;
%%is_pandigital10(D, _D2, _D3, _D4, _D5, _D6, _D7, D, _D9, _D10) -> false;
%%is_pandigital10(D, _D2, _D3, _D4, _D5, _D6, _D7, _D8, D, _D10) -> false;
%%is_pandigital10(D, _D2, _D3, _D4, _D5, _D6, _D7, _D8, _D9, D) -> false;
%%is_pandigital10(_D1, D, D, _D4, _D5, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, D, _D3, D, _D5, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, D, _D3, _D4, D, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, D, _D3, _D4, _D5, D, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, D, _D3, _D4, _D5, _D6, D, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, D, _D3, _D4, _D5, _D6, _D7, D, _D9, _D10) -> false;
%%is_pandigital10(_D1, D, _D3, _D4, _D5, _D6, _D7, _D8, D, _D10) -> false;
%%is_pandigital10(_D1, D, _D3, _D4, _D5, _D6, _D7, _D8, _D9, D) -> false;
%%is_pandigital10(_D1, _D2, D, D, _D5, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, D, _D4, D, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, D, _D4, _D5, D, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, D, _D4, _D5, _D6, D, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, D, _D4, _D5, _D6, _D7, D, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, D, _D4, _D5, _D6, _D7, _D8, D, _D10) -> false;
%%is_pandigital10(_D1, _D2, D, _D4, _D5, _D6, _D7, _D8, _D9, D) -> false;
%%is_pandigital10(_D1, _D2, _D3, D, D, _D6, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, D, _D5, D, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, D, _D5, _D6, D, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, D, _D5, _D6, _D7, D, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, D, _D5, _D6, _D7, _D8, D, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, D, _D5, _D6, _D7, _D8, _D9, D) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, D, D, _D7, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, D, _D6, D, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, D, _D6, _D7, D, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, D, _D6, _D7, _D8, D, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, D, _D6, _D7, _D8, _D9, D) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, D, D, _D8, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, D, _D7, D, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, D, _D7, _D8, D, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, D, _D7, _D8, _D9, D) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, _D6, D, D, _D9, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, _D6, D, _D8, D, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, _D6, D, _D8, _D9, D) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, _D6, _D7, D, D, _D10) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, _D6, _D7, D, _D9, D) -> false;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, _D6, _D7, _D8, D, D) -> false;
%%is_pandigital10(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10)
%%    when D1 >= 0, D1 =< 9, D2 >= 0, D2 =< 9, D3 >= 0, D3 =< 9, D4 >= 0, D4 =< 9, D5 >= 0, D5 =< 9, D6 >= 0, D6 =< 9, D7 >= 0, D7 =< 9, D8 >= 0, D8 =< 9, D9 >= 0, D9 =< 9, D10 >= 0, D10 =< 9 -> true;
%%is_pandigital10(_D1, _D2, _D3, _D4, _D5, _D6, _D7, _D8, _D9, _D10) -> false.