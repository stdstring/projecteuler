%% @author std-string

%% Using "problem_022.dat", a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order.
%% Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
%% For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
%% So, COLIN would obtain a score of 938 * 53 = 49714.
%% What is the total of all the name scores in the file?

-module(problem_022).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_022.dat", 871198282}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    NamesList = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    lists:sort(split_names(NamesList)).

-spec solve(PreparedInput :: term()) -> term().
solve(NameList) ->
    {_, Sum} = lists:foldl(fun(Name, {Index, Sum}) -> {Index + 1, Sum + Index * (lists:sum(Name) - length(Name) * ($A - 1))} end, {1, 0}, NameList),
    Sum.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec split_names(NamesList :: [string()]) -> [string()].
split_names(NamesList) -> string:tokens(string:join(NamesList, ""), [$", $,]).