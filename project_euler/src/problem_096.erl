%% 

-module(problem_096).
-export([get_check_data/0, prepare_data/2, solve/1, solve/0]).

-behaviour(numerical_task_behaviour).

-define(SIDE_SIZE, 9).
-define(GRID_SIZE, ?SIDE_SIZE * ?SIDE_SIZE).

-record(grid_case, {name :: string(), grid :: array:array(integer())}).

get_check_data() ->
    [{"problem_096", none}].

prepare_data(ModuleSourceDir, Filename) ->
    Strings = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    convert_data(Strings, []).

solve(Data) ->
    io:format("~p~n", [Data]).

solve() ->
    Strings = load_utils:read_strings("problem_096.dat"),
    Data = convert_data(Strings, []),
    io:format("~p~n", [Data]).

convert_data([], Dest) -> lists:reverse(Dest);
convert_data([Description, Str1, Str2, Str3, Str4, Str5, Str6, Str7, Str8, Str9 | Rest], Dest) ->
    Row1 = lists:map(fun(Char) -> Char - $0 end, Str1),
    Row2 = lists:map(fun(Char) -> Char - $0 end, Str2),
    Row3 = lists:map(fun(Char) -> Char - $0 end, Str3),
    Row4 = lists:map(fun(Char) -> Char - $0 end, Str4),
    Row5 = lists:map(fun(Char) -> Char - $0 end, Str5),
    Row6 = lists:map(fun(Char) -> Char - $0 end, Str6),
    Row7 = lists:map(fun(Char) -> Char - $0 end, Str7),
    Row8 = lists:map(fun(Char) -> Char - $0 end, Str8),
    Row9 = lists:map(fun(Char) -> Char - $0 end, Str9),
    Grid = array:from_list(Row1 ++ Row2 ++ Row3 ++ Row4 ++ Row5 ++ Row6 ++ Row7 ++ Row8 ++ Row9),
    convert_data(Rest, [#grid_case{name = Description, grid = Grid}] ++ Dest).

%% TODO (std_string) : use such approach in all other cases
get_element(X, Y, Grid) ->
    Index = X * ?SIDE_SIZE + Y,
    array:get(Index, Grid).

set_element(X, Y, Value, Grid) ->
    Index = X * ?SIDE_SIZE + Y,
    array:set(Index, Value, Grid).