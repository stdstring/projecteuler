%% @author std-string

%% The Fibonacci sequence is defined by the recurrence relation:
%% F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.
%% It turns out that F(541), which contains 113 digits, is the first Fibonacci number for which the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order).
%% And F(2749), which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.
%% Given that F(k) is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.

-module(problem_104).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(XXX, 100000000).
-define(YYY, 1000000000).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{none, 329468}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) -> process({1, 1, 2}, {?YYY, 1}).

%% ====================================================================
%% Internal functions
%% ====================================================================

process({Current, Prev, K}, {Base, Divider}) when Current =< ?XXX ->
    Next = gen_next_fibonacci(Current, Prev),
    process({Next, Current, K + 1}, {Base, Divider});
process({Current, Prev, K}, {Base, Divider}) ->
    Left = Current div Divider,
    Right = Current rem ?YYY,
    IsLeftPandigital = pandigital_numbers:is_pandigital(numbers:get_digits(Left)),
    IsRightPandigital = pandigital_numbers:is_pandigital(numbers:get_digits(Right)),
    if
        IsLeftPandigital and IsRightPandigital -> K;
        true ->
            Next = gen_next_fibonacci(Current, Prev),
            process({Next, Current, K + 1}, calc_base(Next, {Base, Divider}))
    end.

gen_next_fibonacci(Current, Prev) -> Current + Prev.

calc_base(Number, {Base, Divider}) when Number >= Base -> {Base * 10, Divider * 10};
calc_base(_Number, {Base, Divider}) -> {Base, Divider}.
