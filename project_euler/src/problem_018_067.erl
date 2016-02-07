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

-include("grid_def.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{"problem_018.dat", 1074}, {"problem_067.dat", 7273}].

prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), " ").

solve(Data) ->
    RowCount = length(Data),
    Grid = fill(Data, 1, 1, grid_helper:create(RowCount, RowCount, undef)),
    InitPoints = [{1, 1}],
    ResultPoints = lists:map(fun(Number) -> {RowCount, Number} end, lists:seq(1, RowCount)),
    ValueBuilder = fun(AccValue, PointValue) -> AccValue + PointValue end,
    ValueComparator = fun compare_values/2,
    {Value, _Path} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, ValueBuilder, ValueComparator, fun get_next_points/3),
    Value.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec fill(Data :: [[pos_integer]], Row :: pos_integer(), Column :: pos_integer(), Grid :: grid_type(pos_integer() | 'undef')) ->
    grid_type(pos_integer() | 'undef').
fill([], _Row, _Column, Grid) -> Grid;
fill([[] | Rows], Row, _Column, Grid) -> fill(Rows, Row + 1, 1, Grid);
fill([[Value | RowRest] | Rows], Row, Column, Grid) ->
    fill([RowRest] ++ Rows, Row, Column + 1, grid_helper:set_value(Row, Column, Value, Grid)).

-spec compare_values(LValue :: pos_integer(), RValue :: pos_integer()) -> 'left' | 'equal' | 'right'.
compare_values(LValue, RValue) ->
    if
        LValue > RValue -> left;
        LValue == RValue -> equal;
        LValue < RValue -> right
    end.

-spec get_next_points(Point :: point_type(), RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()].
get_next_points({RowMax, _Column}, RowMax, _ColumnMax) -> [];
get_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row + 1, Column}, {Row + 1, Column + 1}].
