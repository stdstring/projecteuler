%% @author std-string

%% By starting at the top of the triangle below and moving to adjacent numbers on the row below,
%% the maximum total from top to bottom is 23.
%%    3
%%   7 4
%%  2 4 6
%% 8 5 9 3
%% That is, 3 + 7 + 4 + 9 = 23.
%% Find the maximum total from top to bottom in "problem_018.dat", a text file containing a triangle with fifteen rows
%% Find the maximum total from top to bottom in "problem_067.dat", a text file containing a triangle with one-hundred rows

-module(problem_018_067).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_018.dat", 1074}, {"problem_067.dat", 7273}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), " ").

-spec solve(PreparedInput :: term()) -> term().
solve(Data) ->
    RowCount = length(Data),
    Grid = grid:copy(lists:map(fun(Row) -> Row ++ lists:duplicate(RowCount - length(Row), undef) end, Data)),
    InitPoints = [{1, 1}],
    ResultPoints = lists:map(fun(Number) -> {RowCount, Number} end, lists:seq(1, RowCount)),
    ValueBuilder = fun(AccValue, PointValue) -> AccValue + PointValue end,
    ValueComparator = fun compare:compare_desc/2,
    {Value, _Path} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, ValueBuilder, ValueComparator, fun get_next_points/3),
    Value.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_next_points(Point :: grid:point_type(), RowMax :: grid:row_type(), ColumnMax :: grid:column_type()) -> [grid:point_type()].
get_next_points({RowMax, _Column}, RowMax, _ColumnMax) -> [];
get_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row + 1, Column}, {Row + 1, Column + 1}].
