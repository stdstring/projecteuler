%% @author std-string

%% Each new term in the Fibonacci sequence is generated by adding the previous two terms.
%% By starting with 1 and 2, the first 10 terms will be: 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
%% By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

-module(problem_002).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type fibonacci_pair() :: {Previous :: pos_integer(), Current :: pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{4000000-1, 4613732}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxTerm) -> solve_impl({1, 1}, 0, MaxTerm).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec solve_impl(FibonacciPair :: fibonacci_pair(), Sum :: non_neg_integer(), MaxTerm :: pos_integer()) -> non_neg_integer().
solve_impl({_, Current}, Sum, MaxTerm) when Current >= MaxTerm -> Sum;
solve_impl(FibonacciPair, Sum, MaxTerm) ->
    NextFibonacciPair = fibonacci_seq(FibonacciPair),
    solve_impl(NextFibonacciPair, even_sum(NextFibonacciPair, Sum), MaxTerm).

-spec even_sum(FibonacciPair :: fibonacci_pair(), Sum :: non_neg_integer()) -> non_neg_integer().
even_sum({_, Current}, Sum) when Current rem 2 == 0 -> Sum + Current;
even_sum({_, Current}, Sum) when Current rem 2 /= 0 -> Sum.

-spec fibonacci_seq(FibonacciPair :: fibonacci_pair()) -> fibonacci_pair().
fibonacci_seq({Previous, Current}) -> {Current, Current + Previous}.