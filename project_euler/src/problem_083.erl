%% @author std-string

%% In the 5 by 5 matrix (in problem_083_example.dat), the minimal path sum from the top left to the bottom right, by moving left, right, up, and down, is equal to 2297.
%% Find the minimal path sum, in problem_083.dat, a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by moving left, right, up, and down.

-module(problem_083).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-include("grid_def.hrl").

%% TODO (std_string) : move into common
-type compare_result() :: 'left' | 'equal' | 'right'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_083_example.dat", 2297}, {"problem_083.dat", 425185}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), ",").

-spec solve(PreparedInput :: term()) -> term().
solve(GridData) ->
    Grid = grid_helper:create(GridData),
    InitPoints = [{1, 1}],
    ResultPoints = [{grid_helper:get_row_count(Grid), grid_helper:get_column_count(Grid)}],
    ValueBuilder = fun(AccValue, PointValue) -> AccValue + PointValue end,
    ValueComparator = fun compare_values/2,
    {Value, _Path} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, ValueBuilder, ValueComparator, fun get_next_points/3),
    Value.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO (std_string) : move into common
-spec compare_values(LValue :: pos_integer(), RValue :: pos_integer()) -> compare_result().
compare_values(LValue, RValue) ->
    if
        LValue < RValue -> left;
        LValue == RValue -> equal;
        LValue > RValue -> right
    end.

-spec get_next_points(Point :: point_type(), RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()].
get_next_points({RowMax, ColumnMax}, RowMax, ColumnMax) -> [];
get_next_points({1, 1}, _RowMax, _ColumnMax) -> [{1, 2}, {2, 1}];
get_next_points({1, ColumnMax}, _RowMax, ColumnMax) -> [{2, ColumnMax}, {1, ColumnMax - 1}];
get_next_points({RowMax, 1}, RowMax, _ColumnMax) -> [{RowMax - 1, 1}, {RowMax, 2}];
get_next_points({1, Column}, _RowMax, _ColumnMax) -> [{2, Column}, {1, Column - 1}, {1, Column + 1}];
get_next_points({RowMax, Column}, RowMax, _ColumnMax) -> [{RowMax - 1, Column}, {RowMax, Column - 1}, {RowMax, Column + 1}];
get_next_points({Row, 1}, _RowMax, _ColumnMax) -> [{Row - 1, 1}, {Row + 1, 1}, {Row, 2}];
get_next_points({Row, ColumnMax}, _RowMax, ColumnMax) -> [{Row - 1, ColumnMax}, {Row + 1, ColumnMax}, {Row, ColumnMax - 1}];
get_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row - 1, Column}, {Row + 1, Column}, {Row, Column - 1}, {Row, Column + 1}].