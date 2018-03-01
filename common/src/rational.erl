%% @author std-string

-module(rational).
-export([add/2, sub/2, mult/2, divide/2, reverse/1, simplify/1]).

-type non_zero_integer() :: pos_integer() | neg_integer().
-type rational_fraction() :: {Numerator :: integer(), Denominator :: non_zero_integer()}.
-type rational_number() :: IntegerNumber :: integer() | rational_fraction().

-export_type([non_zero_integer/0, rational_fraction/0, rational_number/0]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec add(LeftOperand :: rational_number(), RightOperand :: rational_number()) -> rational_number().
add({N1, D1}, {N2, D2}) when not is_integer(N1); not is_integer(D1); not is_integer(N2); not is_integer(D2) -> error(badarg);
add({_N1, 0}, _R2) -> error(badarg);
add(_R1, {_N2, 0}) -> error(badarg);
add({N1, D}, {N2, D}) -> make_number(N1 + N2, D);
add({0, _D1}, {N2, D2}) -> make_number(N2, D2);
add({N1, D1}, {0, _D2}) -> make_number(N1, D1);
add({N1, D1}, {N2, D2}) -> make_number(N1 * D2 + N2 * D1, D1 * D2);
add({N1, D1}, N2) when not is_integer(N1); not is_integer(D1); not is_integer(N2) -> error(badarg);
add({N1, D1}, N2) -> make_number(N1 + N2 * D1, D1);
add(N1, {N2, D2}) when not is_integer(N1); not is_integer(N2); not is_integer(D2) -> error(badarg);
add(N1, {N2, D2}) -> make_number(N1 * D2 + N2, D2);
add(N1, N2) when not is_integer(N1); not is_integer(N2) -> error(badarg);
add(N1, N2) -> N1 + N2.

-spec sub(LeftOperand :: rational_number(), RightOperand :: rational_number()) -> rational_number().
sub({N1, D1}, {N2, D2}) when not is_integer(N1); not is_integer(D1); not is_integer(N2); not is_integer(D2) -> error(badarg);
sub({_N1, 0}, _R2) -> error(badarg);
sub(_R1, {_N2, 0}) -> error(badarg);
sub({N1, D}, {N2, D}) -> make_number(N1 - N2, D);
sub({0, _D1}, {N2, D2}) -> make_number(-N2, D2);
sub({N1, D1}, {0, _D2}) -> make_number(N1, D1);
sub({N1, D1}, {N2, D2}) -> make_number(N1 * D2 - N2 * D1, D1 * D2);
sub({N1, D1}, N2) when not is_integer(N1); not is_integer(D1); not is_integer(N2) -> error(badarg);
sub({N1, D1}, N2) -> make_number(N1 - N2 * D1, D1);
sub(N1, {N2, D2}) when not is_integer(N1); not is_integer(N2); not is_integer(D2) -> error(badarg);
sub(N1, {N2, D2}) -> make_number(N1 * D2 - N2, D2);
sub(N1, N2) when not is_integer(N1); not is_integer(N2) -> error(badarg);
sub(N1, N2) -> N1 - N2.

-spec mult(LeftOperand :: rational_number(), RightOperand :: rational_number()) -> rational_number().
mult({N1, D1}, {N2, D2}) when not is_integer(N1); not is_integer(D1); not is_integer(N2); not is_integer(D2) -> error(badarg);
mult({_N1, 0}, _R2) -> error(badarg);
mult(_R1, {_N2, 0}) -> error(badarg);
mult({0, _D1}, {_N2, _D2}) -> 0;
mult({_N1, _D1}, {0, _D2}) -> 0;
mult({N1, D1}, {N2, D2}) -> make_number(N1 * N2, D1 * D2);
mult({N1, D1}, N2) when not is_integer(N1); not is_integer(D1); not is_integer(N2) -> error(badarg);
mult({N1, D1}, N2) -> make_number(N1 * N2, D1);
mult(N1, {N2, D2}) when not is_integer(N1); not is_integer(N2); not is_integer(D2) -> error(badarg);
mult(N1, {N2, D2}) -> make_number(N1 * N2, D2);
mult(N1, N2) when not is_integer(N1); not is_integer(N2) -> error(badarg);
mult(N1, N2) -> N1 * N2.

-spec divide(LeftOperand :: rational_number(), RightOperand :: rational_number()) -> rational_number().
divide({N1, D1}, {N2, D2}) when not is_integer(N1); not is_integer(D1); not is_integer(N2); not is_integer(D2) -> error(badarg);
divide({_N1, 0}, _R2) -> error(badarg);
divide(_R1, {_N2, 0}) -> error(badarg);
divide({_N1, _D1}, {0, _D2}) -> error(badarg);
divide({0, _D1}, {_N2, _D2}) -> 0;
divide({N1, D1}, {N2, D2}) -> make_number(N1 * D2, D1 * N2);
divide({N1, D1}, N2) when not is_integer(N1); not is_integer(D1); not is_integer(N2) -> error(badarg);
divide({_N1, _D1}, 0) -> error(badarg);
divide({N1, D1}, N2) -> make_number(N1 , D1 * N2);
divide(N1, {N2, D2}) when not is_integer(N1); not is_integer(N2); not is_integer(D2) -> error(badarg);
divide(N1, {N2, D2}) -> make_number(N1 * D2, N2);
divide(N1, N2) when not is_integer(N1); not is_integer(N2) -> error(badarg);
divide(N1, N2) -> make_number(N1, N2).

-spec reverse(Operand :: rational_number()) -> rational_number().
reverse({N, D}) when not is_integer(N); not is_integer(D) -> error(badarg);
reverse({_N, 0}) -> error(badarg);
reverse({N, D}) -> make_number(D, N);
reverse(N) when not is_integer(N) -> error(badarg);
reverse(0) -> error(badarg);
reverse(N) -> {1, N}.

-spec simplify(Operand :: rational_number()) -> rational_number().
simplify({N, D}) when not is_integer(N); not is_integer(D) -> error(badarg);
simplify({_N, 0}) -> error(badarg);
simplify({N, D}) ->
    GcdValue = number_dividers:calc_gcd(N, D),
    SimplifiedN = N div GcdValue,
    SimplifiedD = D div GcdValue,
    case SimplifiedD of
        1 -> SimplifiedN;
        _Other -> {SimplifiedN, SimplifiedD}
    end;
simplify(N) when not is_integer(N) -> error(badarg);
simplify(N) -> N.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec make_number(Numerator :: integer(), Denominator :: non_zero_integer()) -> rational_number().
make_number(0, _D) -> 0;
make_number(N, 1) -> N;
make_number(N, -1) -> -N;
make_number(N, D) when N rem D == 0 -> N div D;
make_number(N, D) when D rem N == 0 ->
    Denominator = D div N,
    if
        Denominator < 0 -> {-1, -Denominator};
        Denominator > 0 -> {1, Denominator}
    end;
make_number(N, D) when D < 0 -> {-N, -D};
make_number(N, D) -> {N, D}.