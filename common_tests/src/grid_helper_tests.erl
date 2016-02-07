-module(grid_helper_tests).

-include_lib("eunit/include/eunit.hrl").

-include("grid_def.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_from_data_test_() ->
    [{"single number grid", ?_assert(check_grid([[666]]))},
     {"single row grid", ?_assert(check_grid([[11, 12, 13]]))},
     {"single column grid", ?_assert(check_grid([[11], [12], [13]]))},
     {"rectangle grid", ?_assert(check_grid([[11, 12, 13], [14, 15, 16]]))},
     {"square grid", ?_assert(check_grid([[11, 12], [13, 14]]))},
     {"rectangle atom grid", ?_assert(check_grid([[aa, bb, cc], [dd, ee, ff]]))}].

create_test_() ->
    [{"1 x 1 grid", ?_assert(check_grid([[undef]], grid_helper:create(1, 1, undef)))},
     {"2 x 1 grid", ?_assert(check_grid([[undef], [undef]], grid_helper:create(2, 1, undef)))},
     {"1 x 2 grid", ?_assert(check_grid([[undef, undef]], grid_helper:create(1, 2, undef)))},
     {"2 x 2 grid", ?_assert(check_grid([[undef, undef], [undef, undef]], grid_helper:create(2, 2, undef)))},
     {"2 x 3 grid", ?_assert(check_grid([[undef, undef, undef], [undef, undef, undef]], grid_helper:create(2, 3, undef)))}].

get_row_count_test_() ->
    [{"single item grid", ?_assertEqual(1, grid_helper:get_row_count(grid_helper:create([[666]])))},
     {"single row grid", ?_assertEqual(1, grid_helper:get_row_count(grid_helper:create([[666, 777]])))},
     {"single column grid", ?_assertEqual(2, grid_helper:get_row_count(grid_helper:create([[666], [777]])))},
     {"rectangle grid", ?_assertEqual(2, grid_helper:get_row_count(grid_helper:create([[6, 6, 6], [7, 7, 7]])))},
     {"square grid", ?_assertEqual(2, grid_helper:get_row_count(grid_helper:create([[6, 6], [7, 7]])))},
     {"jagged grid", ?_assertEqual(2, grid_helper:get_row_count(grid_helper:create([[6, 6, 6], [7, 7]])))}].

get_column_count_test_() ->
    [{"single item grid", ?_assertEqual(1, grid_helper:get_column_count(grid_helper:create([[666]])))},
     {"single row grid", ?_assertEqual(2, grid_helper:get_column_count(grid_helper:create([[666, 777]])))},
     {"single column grid", ?_assertEqual(1, grid_helper:get_column_count(grid_helper:create([[666], [777]])))},
     {"rectangle grid", ?_assertEqual(3, grid_helper:get_column_count(grid_helper:create([[6, 6, 6], [7, 7, 7]])))},
     {"square grid", ?_assertEqual(2, grid_helper:get_column_count(grid_helper:create([[6, 6], [7, 7]])))},
     {"jagged grid", ?_assertEqual(3, grid_helper:get_column_count(grid_helper:create([[6, 6, 6], [7, 7]])))}].

get_value_test_() ->
    Grid = grid_helper:create([[1, 2, 3], [4, 5, 6]]),
    [{"get top-left value", ?_assertEqual(1, grid_helper:get_value(1, 1, Grid))},
     {"get some value", ?_assertEqual(2, grid_helper:get_value(1, 2, Grid))},
     {"get bottom-right value", ?_assertEqual(6, grid_helper:get_value(2, 3, Grid))},
     {"try get value from out-of-range row", ?_assertError(badarg, grid_helper:get_value(100, 1, Grid))},
     {"try get value from out-of-range column", ?_assertError(badarg, grid_helper:get_value(1, 100, Grid))},
     {"try get value from out-of-range row and column", ?_assertError(badarg, grid_helper:get_value(100, 100, Grid))}].

set_value_test_() ->
    Grid = grid_helper:create([[1, 2, 3], [4, 5, 6]]),
    [{"set top-left value", ?_assertEqual(666, process_set_value(1, 1, 666, Grid))},
     {"set some value", ?_assertEqual(666, process_set_value(1, 2, 666, Grid))},
     {"set bottom-right value", ?_assertEqual(666, process_set_value(2, 3, 666, Grid))},
     {"try set value from out-of-range row", ?_assertError(badarg, process_set_value(100, 1, 666, Grid))},
     {"try set value from out-of-range column", ?_assertError(badarg, process_set_value(1, 100, 666, Grid))},
     {"try set value from out-of-range row and column", ?_assertError(badarg, process_set_value(100, 100, 666, Grid))}].


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_grid(GridData :: [[term()]]) -> boolean().
check_grid(GridData) -> check_grid(GridData, grid_helper:create(GridData)).

-spec check_grid(GridData :: [[term()]], Grid :: grid_type(term())) -> boolean().
check_grid(GridData, Grid) -> check_grid(GridData, 1, 1, Grid).

-spec check_grid(GridData :: [[term()]], Row :: pos_integer(), Column :: pos_integer(), Grid :: grid_type()) -> boolean().
check_grid([], _Row, _Column, _Grid) -> true;
check_grid([[] | Rows], Row, _Column, Grid) ->
    check_grid(Rows, Row + 1, 1, Grid);
check_grid([[Value | RowRest] | Rows], Row, Column, Grid) ->
    GridRow = array:get(Row - 1, Grid),
    GridValue = array:get(Column - 1, GridRow),
    if
        Value /= GridValue -> false;
        Value == GridValue -> check_grid([RowRest] ++ Rows, Row, Column + 1, Grid)
    end.

-spec process_set_value(Row :: pos_integer(), Column :: pos_integer(), Value :: term(), Grid :: grid_type()) -> term().
process_set_value(Row, Column, Value, Grid) ->
    grid_helper:get_value(Row, Column, grid_helper:set_value(Row, Column, Value, Grid)).