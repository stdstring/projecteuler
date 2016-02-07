%% @author std-string

%% Consider quadratic Diophantine equations of the form:
%% x^2 – D * y^2 = 1
%% For example, when D = 13, the minimal solution in x is 649^2 – 13 * 180^2 = 1.
%% It can be assumed that there are no solutions in positive integers when D is square.
%% By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
%% 3^2 – 2 * 2^2 = 1
%% 2^2 – 3 * 1^2 = 1
%% 9^2 – 5 * 4^2 = 1
%% 5^2 – 6 * 2^2 = 1
%% 8^2 – 7 * 3^2 = 1
%% Hence, by considering minimal solutions in x for D <= 7, the largest x is obtained when D = 5.
%% Find the value of D <= 1000 in minimal solutions of x for which the largest value of x is obtained.

-module(problem_066).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(NUMBER_START, 2).
-define(ROOT_START, 1).

-type value_pair() :: {Current :: integer(), Prev :: integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{7, 5}, {1000, 661}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) ->
    process_numbers(?NUMBER_START, ?ROOT_START, MaxNumber, {0, 0}).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(Number :: integer(),
                      Root :: integer(),
                      MaxNumber :: integer(),
                      SavedData :: {SavedNumber :: integer(), SavedMaxX :: integer()}) ->
    integer().
process_numbers(Number, _Root, MaxNumber, {SavedNumber, _SavedMaxX}) when Number > MaxNumber -> SavedNumber;
process_numbers(Number, Root, MaxNumber, {SavedNumber, SavedMaxX}) when Number == ((Root + 1) * (Root + 1)) ->
    process_numbers(Number + 1, Root + 1, MaxNumber, {SavedNumber, SavedMaxX});
process_numbers(Number, Root, MaxNumber, {SavedNumber, SavedMaxX}) ->
    {X, _Y} = find_solution(Number, Root),
    if
        X > SavedMaxX -> process_numbers(Number + 1, Root, MaxNumber, {Number, X});
        X =< SavedMaxX -> process_numbers(Number + 1, Root, MaxNumber, {SavedNumber, SavedMaxX})
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
-spec find_solution(D :: integer(), A0 :: integer()) -> {X :: integer(), Y :: integer()}.
find_solution(D, A0) ->
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