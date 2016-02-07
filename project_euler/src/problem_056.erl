%% A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one followed by two-hundred zeros.
%% Despite their size, the sum of the digits in each number is only 1.
%% Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?

-module(problem_056).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MIN_BASE, 2).
-define(MIN_EXP, 2).

get_check_data() ->
    [{{100, 100}, 972}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve({MaxBase, MaxExp}) ->
    process_base(?MIN_BASE, MaxBase, MaxExp, 1).

-spec process_base(Base :: pos_integer(), MaxBase :: pos_integer(), MaxExp :: pos_integer(), Result :: pos_integer()) -> pos_integer().
process_base(Base, MaxBase, _MaxExp, Result) when Base > MaxBase -> Result;
process_base(Base, MaxBase, MaxExp, Result) ->
    NewResult = process_exp(Base, ?MIN_EXP, MaxExp, Result),
    process_base(Base + 1, MaxBase, MaxExp, NewResult).

-spec process_exp(Base :: pos_integer(), Exp :: pos_integer(), MaxExp :: pos_integer(), Result :: pos_integer()) -> pos_integer().
process_exp(_Base, Exp, MaxExp, Result) when Exp > MaxExp -> Result;
process_exp(Base, Exp, MaxExp, Result) ->
    Value = numbers:power(Base, Exp),
    DigitsSum = calc_digits_sum(Value),
    if
        DigitsSum =< Result -> process_exp(Base, Exp + 1, MaxExp, Result);
        DigitsSum > Result -> process_exp(Base, Exp + 1, MaxExp, DigitsSum)
    end.

-spec calc_digits_sum(Number :: pos_integer()) -> pos_integer().
calc_digits_sum(Number) ->
    lists:sum(numbers:get_digits(Number)).