%% @author std-string

%% 

-module(problem_060).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{none, none}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================