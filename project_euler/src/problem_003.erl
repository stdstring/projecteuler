%% The prime factors of 13195 are 5, 7, 13 and 29.
%% What is the largest prime factor of the number 600851475143 ?

-module(problem_003).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{13195, 29}, {600851475143, 6857}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(Number) -> lists:last(traverse_number(Number)).

traverse_number(Number) ->
    RestNumber = clear_from_divider(Number, 2),
    if
        RestNumber == Number -> traverse_number(Number, 3, Number div 2, []);
        RestNumber /= Number -> traverse_number(RestNumber, 3, RestNumber, [2])
    end.

traverse_number(_, Current, Bound, DividersList) when Current > Bound -> DividersList;
traverse_number(Number, Current, _, DividersList) when Number rem Current == 0 ->
    RestNumber = clear_from_divider(Number, Current),
    IsPrime = lists:all(fun(Item) -> Current rem Item /= 0 end, DividersList),
    if
        IsPrime == true -> traverse_number(RestNumber, Current + 2, RestNumber, DividersList ++ [Current]);
        IsPrime == false -> traverse_number(RestNumber, Current + 2, RestNumber, DividersList)
    end;
traverse_number(Number, Current, Bound, DividersList) -> traverse_number(Number, Current + 2, Bound, DividersList).

clear_from_divider(Number, Divider) when Number rem Divider /= 0 -> Number;
clear_from_divider(Number, Divider) when Number rem Divider == 0 -> clear_from_divider(Number div Divider, Divider).