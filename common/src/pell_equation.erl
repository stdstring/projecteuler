%% @author std-string

-module(pell_equation).
-export([find_first_solution/2, find_n_solution/4]).

-type value_pair() :: {Current :: integer(), Prev :: integer()}.
-type solution() :: {X :: integer(), Y :: integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec find_first_solution(D :: integer(), C :: integer()) -> solution() | no_return().
find_first_solution(_D, C) when (C /= 1) and (C /= -1) -> error(not_supported);
find_first_solution(D, C) ->
    check_d_value(D),
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

-spec find_n_solution(FirstSolution :: solution(), D :: pos_integer(), C :: integer(), N :: pos_integer()) -> solution() | no_return().
find_n_solution(_FirstSolution, _D, C, _N) when (C /= 1) and (C /= -1) -> error(not_supported);
find_n_solution(_FirstSolution, _D, C, N) when (C == -1) and (N rem 2 /= 1) -> error(badarg);
find_n_solution(FirstSolution, D, _C, N) ->
    check_d_value(D),
    {find_x(FirstSolution, D, N), find_y(FirstSolution, D, N)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_d_value(D :: integer()) -> 'ok' | no_return().
check_d_value(D) when D =< 0 -> error(badarg);
check_d_value(D) ->
    SqrtValue = math:sqrt(D),
    BottomValue = trunc(SqrtValue),
    TopValue = round(SqrtValue),
    if
        (BottomValue * BottomValue == D) or (TopValue * TopValue == D) -> error(badarg);
        true -> ok
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