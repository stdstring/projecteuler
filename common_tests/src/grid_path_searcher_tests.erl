-module(grid_path_searcher_tests).

-include_lib("eunit/include/eunit.hrl").

-include("grid_def.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

two_way_test() ->
    Grid = generate_source_grid(),
    InitPoints = [{1, 1}],
    ResultPoints = [{grid_helper:get_row_count(Grid), grid_helper:get_column_count(Grid)}],
    {ActualValue, ActualPath} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, fun build_next_value/2, fun compare_values_on_min/2, fun get_two_way_next_points/3),
    ?assertEqual(2427, ActualValue),
    ?assertEqual([{1, 1}, {2, 1}, {2, 2}, {2, 3}, {3, 3}, {3, 4}, {4, 4}, {5, 4}, {5, 5}], ActualPath).

three_way_test() ->
    Grid = generate_source_grid(),
    RowMax = grid_helper:get_row_count(Grid),
    ColumnMax = grid_helper:get_column_count(Grid),
    InitPoints = lists:map(fun(Number) -> {Number, 1} end, lists:seq(1, RowMax)),
    ResultPoints = lists:map(fun(Number) -> {Number, ColumnMax} end, lists:seq(1, RowMax)),
    {ActualValue, ActualPath} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, fun build_next_value/2, fun compare_values_on_min/2, fun get_three_way_next_points/3),
    ?assertEqual(994, ActualValue),
    ?assertEqual([{2, 1}, {2, 2}, {2, 3}, {1, 3}, {1, 4}, {1, 5}], ActualPath).

four_way_test() ->
    Grid = generate_source_grid(),
    InitPoints = [{1, 1}],
    ResultPoints = [{grid_helper:get_row_count(Grid), grid_helper:get_column_count(Grid)}],
    {ActualValue, ActualPath} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, fun build_next_value/2, fun compare_values_on_min/2, fun get_four_way_next_points/3),
    ?assertEqual(2297, ActualValue),
    ?assertEqual([{1, 1}, {2, 1}, {2, 2}, {2, 3}, {1, 3}, {1, 4}, {1, 5}, {2, 5}, {3, 5}, {3, 4}, {4, 4}, {5, 4}, {5, 5}], ActualPath).

direct_triangle_test() ->
    Grid = generate_triangle_source_grid(),
    InitPoints = [{1, 1}],
    RowMax = grid_helper:get_row_count(Grid),
    ResultPoints = lists:map(fun(Number) -> {RowMax, Number} end, lists:seq(1, RowMax)),
    {ActualValue, ActualPath} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, fun build_next_value/2, fun compare_values_on_max/2, fun get_direct_triangle_next_points/3),
    ?assertEqual(23, ActualValue),
    ?assertEqual([{1, 1}, {2, 1}, {3, 2}, {4, 3}], ActualPath).

reverse_triangle_test() ->
    Grid = generate_triangle_source_grid(),
    RowMax = grid_helper:get_row_count(Grid),
    InitPoints = lists:map(fun(Number) -> {RowMax, Number} end, lists:seq(1, RowMax)),
    ResultPoints = [{1, 1}],
    {ActualValue, ActualPath} = grid_path_searcher:search(Grid, InitPoints, ResultPoints, fun build_next_value/2, fun compare_values_on_max/2, fun get_reverse_triangle_next_points/3),
    ?assertEqual(23, ActualValue),
    ?assertEqual([{4, 3}, {3, 2}, {2, 1}, {1, 1}], ActualPath).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_source_grid() -> grid_type(pos_integer()).
generate_source_grid() ->
    grid_helper:create([[131, 673, 234, 103, 18], [201, 96, 342, 965, 150], [630, 803, 746, 422, 111], [537, 699, 497, 121, 956], [805, 732, 524, 37, 331]]).

-spec build_next_value(AccValue :: pos_integer(), PointValue :: pos_integer()) -> pos_integer().
build_next_value(AccValue, PointValue) -> AccValue + PointValue.

-spec compare_values_on_min(LValue :: pos_integer(), RValue :: pos_integer()) -> 'left' | 'equal' | 'right'.
compare_values_on_min(LValue, RValue) ->
    if
        LValue < RValue -> left;
        LValue == RValue -> equal;
        LValue > RValue -> right
    end.

-spec get_two_way_next_points(Point :: point_type(), RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()].
get_two_way_next_points({RowMax, ColumnMax}, RowMax, ColumnMax) -> [];
get_two_way_next_points({RowMax, Column}, RowMax, _ColumnMax) -> [{RowMax, Column + 1}];
get_two_way_next_points({Row, ColumnMax}, _RowMax, ColumnMax) -> [{Row + 1, ColumnMax}];
get_two_way_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row + 1, Column}, {Row, Column + 1}].

-spec get_three_way_next_points(Point :: point_type(), _RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()].
get_three_way_next_points({_Row, ColumnMax}, _RowMax, ColumnMax) -> [];
get_three_way_next_points({1, Column}, _RowMax, _ColumnMax) -> [{1, Column + 1}, {2, Column}];
get_three_way_next_points({RowMax, Column}, RowMax, _ColumnMax) -> [{RowMax, Column + 1}, {RowMax - 1, Column}];
get_three_way_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row + 1, Column}, {Row - 1, Column}, {Row, Column + 1}].

-spec get_four_way_next_points(Point :: point_type(), RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()].
get_four_way_next_points({RowMax, ColumnMax}, RowMax, ColumnMax) -> [];
get_four_way_next_points({1, 1}, _RowMax, _ColumnMax) -> [{1, 2}, {2, 1}];
get_four_way_next_points({1, ColumnMax}, _RowMax, ColumnMax) -> [{2, ColumnMax}, {1, ColumnMax - 1}];
get_four_way_next_points({RowMax, 1}, RowMax, _ColumnMax) -> [{RowMax - 1, 1}, {RowMax, 2}];
get_four_way_next_points({1, Column}, _RowMax, _ColumnMax) -> [{2, Column}, {1, Column - 1}, {1, Column + 1}];
get_four_way_next_points({RowMax, Column}, RowMax, _ColumnMax) -> [{RowMax - 1, Column}, {RowMax, Column - 1}, {RowMax, Column + 1}];
get_four_way_next_points({Row, 1}, _RowMax, _ColumnMax) -> [{Row - 1, 1}, {Row + 1, 1}, {Row, 2}];
get_four_way_next_points({Row, ColumnMax}, _RowMax, ColumnMax) -> [{Row - 1, ColumnMax}, {Row + 1, ColumnMax}, {Row, ColumnMax - 1}];
get_four_way_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row - 1, Column}, {Row + 1, Column}, {Row, Column - 1}, {Row, Column + 1}].

-spec generate_triangle_source_grid() -> grid_type(pos_integer()).
generate_triangle_source_grid() ->
    grid_helper:create([[3, 0, 0, 0], [7, 4, 0, 0], [2, 4, 6, 0], [8, 5, 9, 3]]).

-spec compare_values_on_max(LValue :: pos_integer(), RValue :: pos_integer()) -> 'left' | 'equal' | 'right'.
compare_values_on_max(LValue, RValue) ->
    if
        LValue > RValue -> left;
        LValue == RValue -> equal;
        LValue < RValue -> right
    end.

-spec get_direct_triangle_next_points(Point :: point_type(), RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()].
get_direct_triangle_next_points({RowMax, _Column}, RowMax, _ColumnMax) -> [];
get_direct_triangle_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row + 1, Column}, {Row + 1, Column + 1}].

-spec get_reverse_triangle_next_points(Point :: point_type(), RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()].
get_reverse_triangle_next_points({1, 1}, _RowMax, _ColumnMax) -> [];
get_reverse_triangle_next_points({Row, 1}, _RowMax, _ColumnMax) -> [{Row - 1, 1}];
get_reverse_triangle_next_points({Row, Row}, _RowMax, _ColumnMax) -> [{Row - 1, Row - 1}];
get_reverse_triangle_next_points({Row, Column}, _RowMax, _ColumnMax) -> [{Row - 1, Column - 1}, {Row - 1, Column}].