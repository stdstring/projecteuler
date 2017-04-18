%% @author std-string

-module(pell_equation).
-export([find_first_solution/2, find_n_solution/4]).

-type value_pair() :: {Current :: integer(), Prev :: integer()}.
-type solution() :: {X :: integer(), Y :: integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

%% Algorithm (from http://mathworld.wolfram.com/PellEquation.html):
%% Pell equation of the form x^2 - D * y^2 = 1 can be solved by finding the continued fraction [a(0), a(1), ...] of sqrt(D).
%% Let ignore the trivial solution x = 1, y = 0.
%% Let p(n) / q(n) denote the n-th convergent [a(0), a(1), ..., a(n)],
%% then we will have solved Pell equation if we can find a convergent which obeys the identity p(n)^2 - D * q(n)^2 = (-1)^(n+1).
%% Amazingly, this turns out to always be possible as a result of the fact that the continued fraction of a quadratic surd always becomes periodic at some term a(r+1),
%% where a(r+1) = 2 * a(0), i.e., sqrt(D) = [a(0), a(1), ..., a(r), 2 * a(0), ...].
%% To compute the continued fraction convergents to sqrt(D), use the usual recurrence relations:
%% a(0) = [sqrt(D)]
%% p(0) = a(0)
%% p(1) = a(0) * a(1) + 1
%% p(n) = a(n) * p(n-1) + p(n-2)
%% q(0) = 1
%% q(1) = a1
%% q(n) = a(n) * q(n-1) + q(n-2)
%% where [x] is the floor function. For reasons to be explained shortly, also compute the two additional quantities P(n) and Q(n) defined by
%% P(0) = 0
%% P(1) = a(0)
%% Pn = a(n-1) * Q(n-1) - P(n-1)
%% Q(0) = 1
%% Q(1) = D - a(0) * a(0)
%% Q(n) = (D - P(n) * P(n)) / Q(n-1)
%% a(n) = [(a(0) + P(n)) / Q(n)]
%% Let a(r+1) = 2 * a(0) be the term at which the continued fraction becomes periodic (which will always happen for a quadratic surd).
%% For the Pell equation x^2 - D * y^2 = 1 with r odd, (-1)^(r+1) is positive and the solution in terms of smallest integers is x = p(r) and y = q(r),
%% where p(r) / q(r) is the r-th convergent.
%% If r is even, then (-1)^(r+1) is negative, but p(2r+1)^2 - D * q(2r+1)^2 = 1, so the solution in smallest integers is x = p(2r+1), y = q(2r+1).
%% The equation x^2 - D * y^2 = -1 can be solved analogously to the equation with +1 on the right side if r is even, but has no solution if r is odd:
%% x = p(r) and y = q(r) for r is even, no solution for r is odd
-spec find_first_solution(D :: integer(), C :: integer()) -> solution() | 'undef' | no_return().
find_first_solution(_D, C) when (C /= 1) and (C /= -1) -> error(not_supported);
find_first_solution(D, _C) when D < 0 -> error(badarg);
find_first_solution(D, C) ->
    case is_perfect_square(D) of
        true -> undef;
        false -> find_first_solution_impl(D, C)
    end.

%% Algorithm (from http://mathworld.wolfram.com/PellEquation.html):
%% Given one solution (x,y)=(p,q) (which can be found as above), a whole family of solutions can be found by taking each side to the n-th power,
%% x^2 - D * y^2= (p^2 - D * q^2)^n = 1.
%% Factoring gives the following:
%% (x + sqrt(D) * y) * (x - sqrt(D) * y) = (p + sqrt(D) * q)^n * (p - sqrt(D) * q)^n
%% and
%% x + sqrt(D) * y  = (p + sqrt(D) * q)^n
%% x - sqrt(D) * y  = (p - sqrt(D) * q)^n,
%% which gives the family of solutions
%% x = ((p + q * sqrt(D))^n + (p - q * sqrt(D))^n) / 2
%% y = ((p + q * sqrt(D))^n - (p - q * sqrt(D))^n) / (2 * sqrt(D)).
%% These solutions also hold for x^2 - D * y^2 = -1, except that n can take on only odd values.
%% Binomial theorem (from https://en.wikipedia.org/wiki/Binomial_theorem):
%% (a + b)^n = C(0, n) * a^n + C(1, n) * a^(n-1) * b + ... + C(k, n) * a^(n-k) * b^k + ... + C(n, n) * b^n
%% where C(k, n) = n! / (k! * (n - k)!) - Binomial coefficient
%% (a + b)^n + (a - b)^n = C(0, n) * a^n + C(1, n) * a^(n-1) * b + C(2, n) * a^(n-2) * b^2 + ... + C(0, n) * a^n - C(1, n) * a^(n-1) * b + C(2, n) * a^(n-2) * b^2 + ... =
%% 2 * C(0, n) * a^n  + 2 * C(2, n) * a^(n-2) * b^2 + ...
%% (a + b)^n + (a - b)^n = C(0, n) * a^n + C(1, n) * a^(n-1) * b + C(2, n) * a^(n-2) * b^2 + ... - C(0, n) * a^n + C(1, n) * a^(n-1) * b - C(2, n) * a^(n-2) * b^2 + ... =
%% 2 * C(1, n) * a^(n-1) * b + 2 * C(3, n) * a^(n-3) * b^3 + ...
%% from this is following:
%% x = ((p + q * sqrt(D))^n + (p - q * sqrt(D))^n) / 2 =
%% (2 * C(0, n) * p^n  + 2 * C(2, n) * p^(n-2) * q^2 * D + ... + 2 * C(k, n) * p^(n-k) * q^k * D^(k/2) [k is even] + ...)/2 = 
%% C(0, n) * p^n  + C(2, n) * p^(n-2) * q^2 * D + ... + C(k, n) * p^(n-k) * q^k * D^(k/2) [k is even] + ...
%% y = ((p + q * sqrt(D))^n - (p - q * sqrt(D))^n) / (2 * sqrt(D)) =
%% (2 * C(1, n) * p^(n-1) * q * D^(1/2) + 2 * C(3, n) * p^(n-3) * q^3 * D^(3/2) + ...) / (2 * sqrt(D)) =
%% C(1, n) * p^(n-1) * q + 2 * C(3, n) * p^(n-3) * q^3 * D + ... + C(k, n) * p^(n-k) * q^k * D^((k-1)/2) [k is odd] + ...
-spec find_n_solution(FirstSolution :: solution(), D :: pos_integer(), C :: integer(), N :: pos_integer()) -> solution() | 'undef' | no_return().
find_n_solution(_FirstSolution, _D, C, _N) when (C /= 1) and (C /= -1) -> error(not_supported);
find_n_solution(_FirstSolution, _D, C, N) when (C == -1) and (N rem 2 /= 1) -> error(badarg);
find_n_solution(_FirstSolution, D, _C, _N) when D < 0 -> error(badarg);
find_n_solution(FirstSolution, D, _C, N) ->
    case is_perfect_square(D) of
        true -> undef;
        false -> {find_x(FirstSolution, D, N), find_y(FirstSolution, D, N)}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO (std_string) : move to another module
-spec is_perfect_square(D :: integer()) -> boolean().
is_perfect_square(D) ->
    SqrtValue = math:sqrt(D),
    BottomValue = trunc(SqrtValue),
    TopValue = round(SqrtValue),
    (BottomValue * BottomValue == D) or (TopValue * TopValue == D).

-spec find_first_solution_impl(D :: integer(), C :: integer()) -> solution() | 'undef'.
find_first_solution_impl(D, C) ->
    A0 = trunc(math:sqrt(D)),
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
    PrevIteration = Iteration - 1,
    if
        (PrevIteration rem 2 == 1) and (C == -1) -> undef;
        (PrevIteration rem 2 == 0) and (C == -1) -> {PNPrev, QNPrev};
        (PrevIteration rem 2 == 1) and (C == 1) -> {PNPrev, QNPrev};
        (PrevIteration rem 2 == 0) and (C == 1) -> find_even_solution(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN, Iteration, 2 * PrevIteration + 1)
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
    {ANext, PNext, QNext, PBigNext, QBigNext} = process_iteration(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN),
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
    {ANext, PNext, QNext, PBigNext, QBigNext} = process_iteration(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN),
    find_even_solution(D, A0, ANext, {PNext, PN}, {QNext, QN}, PBigNext, QBigNext, Iteration + 1, MaxIteration).

-spec process_iteration(D :: integer(),
                        A0 :: integer(),
                        AN :: integer(),
                        PNPair :: value_pair(),
                        QNPair :: value_pair(),
                        PBigN :: integer(),
                        QBigN :: integer()) ->
    {ANext :: integer(), PNext :: integer(), QNext :: integer(), PBigNext :: integer(), QBigNext :: integer()}.
process_iteration(D, A0, AN, {PN, PNPrev}, {QN, QNPrev}, PBigN, QBigN) ->
    PBigNext = AN * QBigN - PBigN,
    QBigNext = (D - PBigNext * PBigNext) div QBigN,
    ANext = (A0 + PBigNext) div QBigNext,
    PNext = ANext * PN + PNPrev,
    QNext = ANext * QN + QNPrev,
    {ANext, PNext, QNext, PBigNext, QBigNext}.

-spec find_x(FirstSolution :: solution(), D :: pos_integer(), N :: pos_integer()) -> integer().
find_x(FirstSolution, D, N) -> find_x(FirstSolution, D, N, 0, 0).

-spec find_x(FirstSolution :: solution(), D :: pos_integer(), N :: pos_integer(), K :: non_neg_integer(), Result :: non_neg_integer()) -> integer().
find_x(_FirstSolution, _D, N, K, Result) when K > N -> Result;
find_x({X0, Y0} = FirstSolution, D, N, K, Result) ->
    Value = calc_binomial_coefficient(N, K) * numbers:power(X0, N - K) * numbers:power(Y0, K) * numbers:power(D, K div 2),
    find_x(FirstSolution, D, N, K + 2, Result + Value).

-spec find_y(FirstSolution :: solution(), D :: pos_integer(), N :: pos_integer()) -> integer().
find_y(FirstSolution, D, N) -> find_y(FirstSolution, D, N, 1, 0).

-spec find_y(FirstSolution :: solution(), D :: pos_integer(), N :: pos_integer(), K :: non_neg_integer(), Result :: non_neg_integer()) -> integer().
find_y(_FirstSolution, _D, N, K, Result) when K > N -> Result;
find_y({X0, Y0} = FirstSolution, D, N, K, Result) ->
    Value = calc_binomial_coefficient(N, K) * numbers:power(X0, N - K) * numbers:power(Y0, K) * numbers:power(D, K div 2),
    find_y(FirstSolution, D, N, K + 2, Result + Value).

%% TODO (std_string) : move to another module
-spec calc_binomial_coefficient(N :: non_neg_integer(), K :: non_neg_integer()) -> pos_integer().
calc_binomial_coefficient(N, K) -> numbers:factorial(N) div (numbers:factorial(K) * numbers:factorial(N - K)).