%% 

-module(problem_096).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{none, none}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) -> none.