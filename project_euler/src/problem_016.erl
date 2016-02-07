%% 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
%% What is the sum of the digits of the number 2^1000?

-module(problem_016).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{{2, 15}, 26}, {{2, 1000}, 1366}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve({Base, Power}) ->
    Result = numbers:power(Base, Power),
    Digits = numbers:get_digits(Result),
    lists:sum(Digits).