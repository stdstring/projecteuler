%% @author std-string

-module(rational).
-export([add/2, reverse/1, simplify/1]).

-type non_zero_integer() :: pos_integer() | neg_integer().
-type rational_fraction() :: {Numerator :: integer(), Denominator :: non_zero_integer()}.
-type rational_number() :: IntegerNumber :: integer() | rational_fraction().

%% ====================================================================
%% API functions
%% ====================================================================

-spec add(LeftOperand :: rational_number(), RightOperand :: rational_number()) -> rational_number().
add({N1, D1}, {N2, D2}) when not is_integer(N1); not is_integer(D1); not is_integer(N2); not is_integer(D2) -> error(badarg);
add({_N1, 0}, _R2) -> error(badarg);
add(_R1, {_N2, 0}) -> error(badarg);
add({N1, D}, {N2, D}) -> {N1 + N2, D};
add({0, _D1}, {N2, D2}) -> {N2, D2};
add({N1, D1}, {0, _D2}) -> {N1, D1};
add({N1, D1}, {N2, D2}) -> {N1 * D2 + N2 * D1, D1 * D2};
add({N1, D1}, N2) when not is_integer(N1); not is_integer(D1); not is_integer(N2) -> error(badarg);
add({N1, D1}, N2) -> {N1 + N2 * D1, D1};
add(N1, {N2, D2}) when not is_integer(N1); not is_integer(N2); not is_integer(D2) -> error(badarg);
add(N1, {N2, D2}) -> {N1 * D2 + N2, D2};
add(N1, N2) when not is_integer(N1); not is_integer(N2) -> error(badarg);
add(N1, N2) -> N1 + N2.

-spec reverse(Operand :: rational_number()) -> rational_number().
reverse({N, D}) when not is_integer(N); not is_integer(D) -> error(badarg);
reverse({_N, 0}) -> error(badarg);
reverse({1, D}) -> D;
reverse({N, D}) -> {D, N};
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