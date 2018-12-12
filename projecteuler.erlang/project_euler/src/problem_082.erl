%% @author std-string

%% In the 5 by 5 matrix (in problem_082_example.dat), the minimal path sum from any cell in the left column to any cell in the right column,
%% by moving left, right, up, and down, is equal to 994.
%% Find the minimal path sum, in problem_082.dat, a text file containing a 80 by 80 matrix, from the left column to the right column.

-module(problem_082).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_082_example.dat", 994}, {"problem_082.dat", 260324}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), ",").

-spec solve(PreparedInput :: term()) -> term().
solve(GridData) ->
    Grid = grid:copy(GridData),
    RowCount = grid:get_row_count(Grid),
    ColumnCount = grid:get_row_count(Grid),
    RowSeq = lists:seq(1, RowCount),
    InitPoints = lists:map(fun(Number) -> {Number, 1} end, RowSeq),
    ResultPoints = lists:map(fun(Number) -> {Number, ColumnCount} end, RowSeq),
    ValueBuilder = fun(AccValue, PointValue) -> AccValue + PointValue end,
    ValueComparator = fun compare:compare_asc/2,
    {Value, _Path} = grid_path:search(Grid, InitPoints, ResultPoints, ValueBuilder, ValueComparator, fun get_next_points/3),
    Value.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_next_points(Point :: grid:point_type(), RowMax :: grid:row_type(), ColumnMax :: grid:column_type()) -> [grid:point_type()].
get_next_points({_Row, ColumnMax}, _RowMax, ColumnMax) -> [];
get_next_points({1, Column}, _RowMax, _ColumnMax) -> [{1, Column + 1}, {2, Column}];
get_next_points({RowMax, Column}, RowMax, _ColumnMax) -> [{RowMax, Column + 1}, {RowMax - 1, Column}];
get_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row + 1, Column}, {Row - 1, Column}, {Row, Column + 1}].