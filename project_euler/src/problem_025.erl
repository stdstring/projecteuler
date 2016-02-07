%% The Fibonacci sequence is defined by the recurrence relation:
%% F(n) = F(n1) + F(n2), where F(1) = 1 and F(2) = 1.
%% Hence the first 12 terms will be:
%% F(1) = 1
%% F(2) = 1
%% F(3) = 2
%% F(4) = 3
%% F(5) = 5
%% F(6) = 8
%% F(7) = 13
%% F(8) = 21
%% F(9) = 34
%% F(10) = 55
%% F(11) = 89
%% F(12) = 144
%% The 12th term, F(12), is the first term to contain three digits.
%% What is the first term in the Fibonacci sequence to contain 1000 digits?

-module(problem_025).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{3, 12}, {1000, 4782}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(DigitsCount) ->
    Bound = numbers:power(10, DigitsCount - 1),
    Generator = fun generate_fibonacci_with_number/1,
    Predicate = fun({{_, Current}, _}) -> Current div Bound == 0 end,
    {_, TermNumber} = skip_while(Generator, Predicate, {{1, 1}, 2}),
    TermNumber.

skip_while(Generator, Predicate, Current) ->
    New = Generator(Current),
    Condition = Predicate(New),
    if
        Condition == true -> skip_while(Generator, Predicate, New);
        Condition == false -> New
    end.

generate_fibonacci_with_number({FibonacciPair, Number}) -> {generate_fibonacci(FibonacciPair), Number + 1}.

generate_fibonacci({Prev, Current}) -> {Current, Current + Prev}.