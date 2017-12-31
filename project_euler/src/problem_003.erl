%% @author std-string

%% The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143 ?

-module(problem_003).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{13195, 29}, {600851475143, 6857}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(Number) -> hd(traverse_number(Number)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec traverse_number(Number :: pos_integer()) -> [pos_integer()].
traverse_number(Number) ->
    RestNumber = clear_from_divider(Number, 2),
    if
        RestNumber == Number -> traverse_number(Number, 3, Number div 2, []);
        RestNumber /= Number -> traverse_number(RestNumber, 3, RestNumber, [2])
    end.

-spec traverse_number(Number :: pos_integer(), Current :: pos_integer(), Bound :: pos_integer(), DividersList :: [pos_integer()]) -> [pos_integer()].
traverse_number(_, Current, Bound, DividersList) when Current > Bound -> DividersList;
traverse_number(Number, Current, _, DividersList) when Number rem Current == 0 ->
    RestNumber = clear_from_divider(Number, Current),
    IsPrime = lists:all(fun(Item) -> Current rem Item /= 0 end, DividersList),
    if
        IsPrime == true -> traverse_number(RestNumber, Current + 2, RestNumber, [Current] ++ DividersList);
        IsPrime == false -> traverse_number(RestNumber, Current + 2, RestNumber, DividersList)
    end;
traverse_number(Number, Current, Bound, DividersList) -> traverse_number(Number, Current + 2, Bound, DividersList).

-spec clear_from_divider(Number :: pos_integer(), Divider :: pos_integer()) -> pos_integer().
clear_from_divider(Number, Divider) when Number rem Divider /= 0 -> Number;
clear_from_divider(Number, Divider) when Number rem Divider == 0 -> clear_from_divider(Number div Divider, Divider).