%% @author std-string

%% In the 5 by 5 matrix (in problem_081_example.dat), the minimal path sum from the top left to the bottom right, by only moving to the right and down, is equal to 2427.
%% Find the minimal path sum, in problem_081.dat, a text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.

-module(problem_081).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-include("grid_def.hrl").

%% TODO (std_string) : move into common
-type compare_result() :: 'left' | 'equal' | 'right'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_081_example.dat", 2427}, {"problem_081.dat", 427337}].

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

-spec get_next_points(Point :: point_type(), XMax :: pos_integer(), YMax :: pos_integer()) -> [point_type()].
get_next_points({RowMax, ColumnMax}, RowMax, ColumnMax) -> [];
get_next_points({RowMax, Column}, RowMax, _ColumnMax) -> [{RowMax, Column + 1}];
get_next_points({Row, ColumnMax}, _RowMax, ColumnMax) -> [{Row + 1, ColumnMax}];
get_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row + 1, Column}, {Row, Column + 1}].