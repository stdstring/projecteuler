%% @author std-string

%% There are exactly ten ways of selecting three from five, 12345:
%% 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
%% In combinatorics, we use the notation, C(5, 3) = 10.
%% In general, C(n, r) = n!/(r!(nr)!), where r <= n, n! = n*(n-1)*...*3*2*1, and 0! = 1.
%% It is not until n = 23, that a value exceeds one-million: C(23, 10) = 1144066.
%% How many, not necessarily distinct, values of  C(n, r), for 1 <= n <= 100, are greater than one-million?

-module(problem_053).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type factorial_storage() :: array:array(Factorial :: pos_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{22, 1000000 + 1}, 0}, {{100, 1000000 + 1}, 4075}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxNumber, Infimum}) ->
    FactorialStorage = prepare_factorials(MaxNumber),
    traverse_koeffs(MaxNumber, Infimum, FactorialStorage).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec traverse_koeffs(Max :: pos_integer(),
                      Infimum :: pos_integer(),
                      FactorialStorage :: factorial_storage()) -> non_neg_integer().
traverse_koeffs(Max, Infimum, FactorialStorage) -> traverse_koeffs(Max, Infimum, 1, 1, 0, FactorialStorage).

-spec traverse_koeffs(Max :: pos_integer(),
                      Infimum :: pos_integer(),
                      N :: pos_integer(),
                      R :: pos_integer(),
                      Count :: non_neg_integer(),
                      FactorialStorage :: factorial_storage()) -> non_neg_integer().
traverse_koeffs(Max, _, N, _, Count, _) when N > Max -> Count;
traverse_koeffs(Max, Infimum, N, R, Count, FactorialStorage) when R > N ->
    traverse_koeffs(Max, Infimum, N + 1, 1, Count, FactorialStorage);
traverse_koeffs(Max, Infimum, N, R, Count, FactorialStorage) ->
    C = calculate_c_koeff(N, R, FactorialStorage),
    if
        C >= Infimum -> traverse_koeffs(Max, Infimum, N, R + 1, Count + 1, FactorialStorage);
        C < Infimum -> traverse_koeffs(Max, Infimum, N, R + 1, Count, FactorialStorage)
    end.

-spec calculate_c_koeff(N :: pos_integer(), R :: pos_integer(), FactorialStorage :: factorial_storage()) -> pos_integer().
calculate_c_koeff(N, R, FactorialStorage) -> array:get(N, FactorialStorage) / (array:get(R, FactorialStorage) * array:get(N - R, FactorialStorage)).

-spec prepare_factorials(MaxNumber :: pos_integer()) ->factorial_storage().
prepare_factorials(MaxNumber) -> array:from_list([numbers:factorial(Number) || Number <- lists:seq(0, MaxNumber)]).