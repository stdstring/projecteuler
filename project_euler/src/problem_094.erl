%% @author std-string

%% It is easily proved that no equilateral triangle exists with integral length sides and integral area.
%% However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
%% We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.
%% Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

-module(problem_094).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type value_pair() :: {Current :: integer(), Prev :: integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{1000000000, 518408346}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxPerimeter) ->
    %% ...
    D = 3,
    A0 = 1,
    {X0, Y0} = find_first_solution(D, A0),
    process({X0, Y0}, X0, Y0, D, 1, MaxPerimeter, 0).

%%solve(MaxPerimeter) ->
%%    MaxSide = round(MaxPerimeter / 3),
%%    process(2, MaxSide, MaxPerimeter, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

process({X, _Y}, X0, Y0, D, N, MaxPerimeter, Sum) ->
    %% x = 3a +- 2
    %%io:format("X=~p, Y=~p X0=~p, Y0=~p~n", [X, Y, X0, Y0]),
    %%A1 = (X - 2) div 3,
    %%C1 = 2 * A1 + 1,
    %%P1 = 2 * C1 + 2 * A1,
    P1 = calc_perimeter(X, 1),
    %%A2 = (X + 2) div 3,
    %%C2 = 2 * A1 - 1,
    %%P2 = 2 * C2 + 2 * A2,
    P2 = calc_perimeter(X, -1),
    if
        (P1 > MaxPerimeter) and (P2 > MaxPerimeter) -> Sum;
        (P1 =< MaxPerimeter) and (P2 > MaxPerimeter) -> Sum + P1;
        (P1 > MaxPerimeter) and (P2 =< MaxPerimeter) -> Sum + P2;
        true -> process(find_solution(X0, Y0, D, N + 1), X0, Y0, D, N + 1, MaxPerimeter, Sum + P1 + P2)
    end.

calc_perimeter(X, Sign) ->
    Value = X - 2 * Sign,
    if
        (Value == 0) or (Value rem 3 /= 0) -> 0;
        true ->
            A = Value div 3,
            C = 2 * A + Sign,
            2 * C  + 2 * A
    end.


%% Algorithm (from http://mathworld.wolfram.com/PellEquation.html):
%% Pell equation of the form x^2 - D * y^2 = 1 can be solved by finding the continued fraction [a0, a1, ...] of D^(1/2).
%% Let ignore the trivial solution x = 1, y = 0.
%% Let pn / qn denote the n-th convergent [a0, a1, ..., an], then we will have solved Pell equation if we can find a convergent which obeys the identity pn^2 -D * qn^2 = (-1)^(n+1).
%% Amazingly, this turns out to always be possible as a result of the fact that the continued fraction of a quadratic surd always becomes periodic at some term a(r+1),
%% where a(r+1) = 2 * a0, i.e., D^(1/2) = [a0, a1, ..., ar, 2 * a0, ...].
%% To compute the continued fraction convergents to D^(1/2), use the usual recurrence relations:
%% a0 = [D^(1/2)]
%% p0 = a0
%% p1 = a0 * a1 + 1
%% pn = an * p(n-1) + p(n-2)
%% q0 = 1
%% q1 = a1
%% qn = an * q(n-1) + q(n-2)
%% where [x] is the floor function. For reasons to be explained shortly, also compute the two additional quantities Pn and Qn defined by
%% P0 = 0
%% P1 = a0
%% Pn = a(n-1) * Q(n-1) - P(n-1)
%% Q0 = 1
%% Q1 = D - a0 * a0
%% Qn = (D - Pn * Pn) / Q(n-1)
%% an = [(a0 + Pn) / Qn]
%% Let a(r+1) = 2 * a0 be the term at which the continued fraction becomes periodic (which will always happen for a quadratic surd).
%% For the Pell equation x^2 - D * y^2 = 1 with r odd, (-1)^(r+1) is positive and the solution in terms of smallest integers is x = pr and y = qr,
%% where pr / qr is the r-th convergent.
%% If r is even, then (-1)^(r+1) is negative, but p(2r+1)^2 - D * q(2r+1)^2 = 1, so the solution in smallest integers is x = p(2r+1), y = q(2r+1).
-spec find_first_solution(D :: integer(), A0 :: integer()) -> {X :: integer(), Y :: integer()}.
find_first_solution(D, A0) ->
    P0 = A0,
    Q0 = 1,
    %%PBig0 = 0,
    %%QBig0 = 1,
    PBig1 = A0,
    QBig1 = D - A0 * A0,
    A1 = (A0 + PBig1) div QBig1,
    P1 = A0 * A1 + 1,
    Q1 = A1,
    {Iteration, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN} = find_period(D, A0, A1, {P1, P0}, {Q1, Q0}, PBig1, QBig1, 1),
    LastPeriodIndex = Iteration - 1,
    if
        (LastPeriodIndex rem 2) == 1 -> {PNPrev, QNPrev};
        true -> find_even_solution(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN, Iteration, 2 * LastPeriodIndex + 1)
    end.

-spec find_period(D :: integer(),
                  A0 :: integer(),
                  AN :: integer(),
                  PNPair :: value_pair(),
                  QNPair :: value_pair(),
                  PBigN :: integer(),
                  QBigN :: integer(),
                  Iteration :: integer()) ->
    {Iteration :: integer(), AN :: integer(), PNPair :: value_pair(), QNPair :: value_pair(), PBigN :: integer(), QBigN :: integer()}.
find_period(_D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN, Iteration) when AN == (2 * A0) ->
    {Iteration, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN};
find_period(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN, Iteration) ->
    {ANext, PNext, QNext, PBigNext, QBigNext} = calc_iteration_step(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN),
    find_period(D, A0, ANext, {PNext, PN}, {QNext, QN}, PBigNext, QBigNext, Iteration + 1).

-spec find_even_solution(D :: integer(),
                         A0 :: integer(),
                         AN :: integer(),
                         PNPair :: value_pair(),
                         QNPair :: value_pair(),
                         PBigN :: integer(),
                         QBigN :: integer(),
                         Iteration :: integer(),
                         MaxIteration :: integer()) ->
    {X :: integer(), Y :: integer()}.
find_even_solution(_D, _A0, _AN, {PN, _PNPrev}, {QN, _QNPrev}, _PBigN, _QBigN, Iteration, Iteration) ->
    {PN, QN};
find_even_solution(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN, Iteration, MaxIteration) ->
    {ANext, PNext, QNext, PBigNext, QBigNext} = calc_iteration_step(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN),
    find_even_solution(D, A0, ANext, {PNext, PN}, {QNext, QN}, PBigNext, QBigNext, Iteration + 1, MaxIteration).

-spec calc_iteration_step(D :: integer(),
                          A0 :: integer(),
                          AN :: integer(),
                          PNPair :: value_pair(),
                          QNPair :: value_pair(),
                          PBigN :: integer(),
                          QBigN :: integer()) ->
    {ANext :: integer(), PNext :: integer(), QNext :: integer(), PBigNext :: integer(), QBigNext :: integer()}.
calc_iteration_step(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN) ->
    PBigNext = AN * QBigN - PBigN,
    QBigNext = (D - PBigNext * PBigNext) div QBigN,
    ANext = (A0 + PBigNext) div QBigNext,
    PNext = ANext * PN + PNPrev,
    QNext = ANext * QN + QNPrev,
    {ANext, PNext, QNext, PBigNext, QBigNext}.

calc_binomial_coefficient(N, K) -> numbers:factorial(N) div (numbers:factorial(K) * numbers:factorial(N - K)).

find_solution(X0, Y0, D, N) -> {find_x(X0, Y0, D, N), find_y(X0, Y0, D, N)}.

find_x(X0, Y0, D, N) -> find_x(X0, Y0, D, N, 0, 0).

find_x(_X0, _Y0, _D, N, K, Result) when K > N -> Result;
find_x(X0, Y0, D, N, K, Result) ->
    Value = calc_binomial_coefficient(N, K) * numbers:power(X0, N - K) * numbers:power(Y0, K) * numbers:power(D, K div 2),
    find_x(X0, Y0, D, N, K + 2, Result + Value).

find_y(X0, Y0, D, N) -> find_y(X0, Y0, D, N, 1, 0).

find_y(_X0, _Y0, _D, N, K, Result) when K > N -> Result;
find_y(X0, Y0, D, N, K, Result) ->
    Value = calc_binomial_coefficient(N, K) * numbers:power(X0, N - K) * numbers:power(Y0, K) * numbers:power(D, K div 2),
    find_y(X0, Y0, D, N, K + 2, Result + Value).