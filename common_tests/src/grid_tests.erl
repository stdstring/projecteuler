%% @author std-string

-module(grid_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_error_test_() ->
    [{"Count of rows isn't integer", ?_assertError(badarg, grid:create(two, 2, 666))},
     {"Count of rows is negative", ?_assertError(badarg, grid:create(-2, 2, 666))},
     {"Count of rows is zero", ?_assertError(badarg, grid:create(0, 2, 666))},
     {"Count of columns isn't integer", ?_assertError(badarg, grid:create(2, two, 666))},
     {"Count of columns is negative", ?_assertError(badarg, grid:create(2, -2, 666))},
     {"Count of rows is zero", ?_assertError(badarg, grid:create(2, 0, 666))}].

create_test_() ->
    [{"Grid 1 x 1", ?_test(check_grid(1, 1, [666], grid:create(1, 1, 666)))},
     {"Grid 1 x 2", ?_test(check_grid(1, 2, [666, 666], grid:create(1, 2, 666)))},
     {"Grid 2 x 1", ?_test(check_grid(2, 1, [666, 666], grid:create(2, 1, 666)))},
     {"Grid 2 x 2", ?_test(check_grid(2, 2, [666, 666, 666, 666], grid:create(2, 2, 666)))}].

copy_flat_data_error_test_() ->
    [{"Count of rows isn't integer", ?_assertError(badarg, grid:copy(two, 2, [1, 2, 3, 4]))},
     {"Count of rows is negative", ?_assertError(badarg, grid:copy(-2, 2, [1, 2, 3, 4]))},
     {"Count of rows is zero", ?_assertError(badarg, grid:copy(0, 2, [1, 2, 3, 4]))},
     {"Count of columns isn't integer", ?_assertError(badarg, grid:copy(2, two, [1, 2, 3, 4]))},
     {"Count of columns is negative", ?_assertError(badarg, grid:copy(2, -2, [1, 2, 3, 4]))},
     {"Count of rows is zero", ?_assertError(badarg, grid:copy(2, 0, [1, 2, 3, 4]))},
     {"Data isn't list", ?_assertError(badarg, grid:copy(2, 2, {1, 2, 3, 4}))},
     {"Bad data length", ?_assertError(badarg, grid:copy(2, 2, [1, 2, 3]))}].

copy_flat_data_test_() ->
    [{"Grid 1 x 1", ?_test(check_grid(1, 1, [666], grid:copy(1, 1, [666])))},
     {"Grid 1 x 2", ?_test(check_grid(1, 2, [666, 777], grid:copy(1, 2, [666, 777])))},
     {"Grid 2 x 1", ?_test(check_grid(2, 1, [777, 666], grid:copy(2, 1, [777, 666])))},
     {"Grid 2 x 2", ?_test(check_grid(2, 2, [666, 777, 888, 999], grid:copy(2, 2, [666, 777, 888, 999])))}].

copy_table_data_error_test_() ->
    [{"Data isn't list", ?_assertError(badarg, grid:copy({1, 2, 3, 4}))},
     {"Data is empty list", ?_assertError(badarg, grid:copy([]))},
     {"First element isn't list", ?_assertError(badarg, grid:copy([{1, 2}, [1, 2]]))},
     {"Second element isn't list", ?_assertError(badarg, grid:copy([[1, 2], {1, 2}]))},
     {"Bad second element length", ?_assertError(badarg, grid:copy([[1, 2], [666]]))}].

copy_table_data_test_() ->
    [{"Grid 1 x 1", ?_test(check_grid(1, 1, [666], grid:copy([[666]])))},
     {"Grid 1 x 2", ?_test(check_grid(1, 2, [666, 777], grid:copy([[666, 777]])))},
     {"Grid 2 x 1", ?_test(check_grid(2, 1, [777, 666], grid:copy([[777], [666]])))},
     {"Grid 2 x 2", ?_test(check_grid(2, 2, [666, 777, 888, 999], grid:copy([[666, 777], [888, 999]])))}].

get_row_count_error_test_() ->
    [{"Bad grid data", ?_assertError(badarg, grid:get_row_count({2, 2, [3, 4, 3, 4]}))}].

get_row_count_test_() ->
    [{"Grid 1 x 1", ?_assertEqual(1, grid:get_row_count(grid:create(1, 1, 666)))},
     {"Grid 1 x 2", ?_assertEqual(1, grid:get_row_count(grid:create(1, 2, 666)))},
     {"Grid 2 x 1", ?_assertEqual(2, grid:get_row_count(grid:create(2, 1, 666)))},
     {"Grid 2 x 2", ?_assertEqual(2, grid:get_row_count(grid:create(2, 2, 666)))}].

get_column_count_error_test_() ->
    [{"Bad grid data", ?_assertError(badarg, grid:get_row_count({2, 2, [3, 4, 3, 4]}))}].

get_column_count_test_() ->
    [{"Grid 1 x 1", ?_assertEqual(1, grid:get_column_count(grid:create(1, 1, 666)))},
     {"Grid 1 x 2", ?_assertEqual(2, grid:get_column_count(grid:create(1, 2, 666)))},
     {"Grid 2 x 1", ?_assertEqual(1, grid:get_column_count(grid:create(2, 1, 666)))},
     {"Grid 2 x 2", ?_assertEqual(2, grid:get_column_count(grid:create(2, 2, 666)))}].

get_value_error_test_() ->
    [{"Bad grid data", ?_assertError(badarg, grid:get_value(1, 1, {2, 2, [3, 4, 3, 4]}))},
     {"Row isn't integer", ?_assertError(badarg, grid:get_value(one, 1, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is negative", ?_assertError(badarg, grid:get_value(-1, 1, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is zero", ?_assertError(badarg, grid:get_value(0, 1, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:get_value(3, 1, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column isn't integer", ?_assertError(badarg, grid:get_value(1, one, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is negative", ?_assertError(badarg, grid:get_value(1, -1, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is zero", ?_assertError(badarg, grid:get_value(1, 0, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:get_value(1, 3, grid:create(2, 2, [1, 2, 3, 4])))}].

get_value_test_() ->
    [{"Grid[1, 1] value", ?_assertEqual(666, grid:get_value(1, 1, grid:copy(2, 2, [666, 777, 888, 999])))},
     {"Grid[1, 2] value", ?_assertEqual(777, grid:get_value(1, 2, grid:copy(2, 2, [666, 777, 888, 999])))},
     {"Grid[2, 1] value", ?_assertEqual(888, grid:get_value(2, 1, grid:copy(2, 2, [666, 777, 888, 999])))},
     {"Grid[2, 2] value", ?_assertEqual(999, grid:get_value(2, 2, grid:copy(2, 2, [666, 777, 888, 999])))}].

get_value_for_point_error_test_() ->
    [{"Bad grid data", ?_assertError(badarg, grid:get_value({1, 1}, {2, 2, [3, 4, 3, 4]}))},
     {"Row isn't integer", ?_assertError(badarg, grid:get_value({one, 1}, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is negative", ?_assertError(badarg, grid:get_value({-1, 1}, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is zero", ?_assertError(badarg, grid:get_value({0, 1}, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:get_value({3, 1}, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column isn't integer", ?_assertError(badarg, grid:get_value({1, one}, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is negative", ?_assertError(badarg, grid:get_value({1, -1}, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is zero", ?_assertError(badarg, grid:get_value({1, 0}, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:get_value({1, 3}, grid:create(2, 2, [1, 2, 3, 4])))}].

get_value_for_point_test_() ->
    [{"Grid[1, 1] value", ?_assertEqual(666, grid:get_value({1, 1}, grid:copy(2, 2, [666, 777, 888, 999])))},
     {"Grid[1, 2] value", ?_assertEqual(777, grid:get_value({1, 2}, grid:copy(2, 2, [666, 777, 888, 999])))},
     {"Grid[2, 1] value", ?_assertEqual(888, grid:get_value({2, 1}, grid:copy(2, 2, [666, 777, 888, 999])))},
     {"Grid[2, 2] value", ?_assertEqual(999, grid:get_value({2, 2}, grid:copy(2, 2, [666, 777, 888, 999])))}].

set_value_error_test_() ->
    [{"Bad grid data", ?_assertError(badarg, grid:set_value(1, 1, 666, {2, 2, [3, 4, 3, 4]}))},
     {"Row isn't integer", ?_assertError(badarg, grid:set_value(one, 1, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is negative", ?_assertError(badarg, grid:set_value(-1, 1, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is zero", ?_assertError(badarg, grid:set_value(0, 1, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:set_value(3, 1, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column isn't integer", ?_assertError(badarg, grid:set_value(1, one, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is negative", ?_assertError(badarg, grid:set_value(1, -1, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is zero", ?_assertError(badarg, grid:set_value(1, 0, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:set_value(1, 3, 666, grid:create(2, 2, [1, 2, 3, 4])))}].

set_value_test_() ->
    [{"Grid[1, 1] value", ?_test(check_grid(2, 2, [13, 666, 666, 666], grid:set_value(1, 1, 13, grid:create(2, 2, 666))))},
     {"Grid[1, 2] value", ?_test(check_grid(2, 2, [666, 13, 666, 666], grid:set_value(1, 2, 13, grid:create(2, 2, 666))))},
     {"Grid[2, 1] value", ?_test(check_grid(2, 2, [666, 666, 13, 666], grid:set_value(2, 1, 13, grid:create(2, 2, 666))))},
     {"Grid[2, 2] value", ?_test(check_grid(2, 2, [666, 666, 666, 13], grid:set_value(2, 2, 13, grid:create(2, 2, 666))))}].

set_value_for_point_error_test_() ->
    [{"Bad grid data", ?_assertError(badarg, grid:set_value({1, 1}, 666, {2, 2, [3, 4, 3, 4]}))},
     {"Row isn't integer", ?_assertError(badarg, grid:set_value({one, 1}, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is negative", ?_assertError(badarg, grid:set_value({-1, 1}, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is zero", ?_assertError(badarg, grid:set_value({0, 1}, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:set_value({3, 1}, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column isn't integer", ?_assertError(badarg, grid:set_value({1, one}, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is negative", ?_assertError(badarg, grid:set_value({1, -1}, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Column is zero", ?_assertError(badarg, grid:set_value({1, 0}, 666, grid:create(2, 2, [1, 2, 3, 4])))},
     {"Row is greater than possible max value", ?_assertError(badarg, grid:set_value({1, 3}, 666, grid:create(2, 2, [1, 2, 3, 4])))}].

set_value_for_point_test_() ->
    [{"Grid[1, 1] value", ?_test(check_grid(2, 2, [13, 666, 666, 666], grid:set_value({1, 1}, 13, grid:create(2, 2, 666))))},
     {"Grid[1, 2] value", ?_test(check_grid(2, 2, [666, 13, 666, 666], grid:set_value({1, 2}, 13, grid:create(2, 2, 666))))},
     {"Grid[2, 1] value", ?_test(check_grid(2, 2, [666, 666, 13, 666], grid:set_value({2, 1}, 13, grid:create(2, 2, 666))))},
     {"Grid[2, 2] value", ?_test(check_grid(2, 2, [666, 666, 666, 13], grid:set_value({2, 2}, 13, grid:create(2, 2, 666))))}].

to_list_error_test_() ->
    [{"Bad grid data", ?_assertError(badarg, grid:to_list({2, 2, [3, 4, 3, 4]}))}].

to_list_test_() ->
    [{"Grid 1 x 1", ?_assertEqual([666], grid:to_list(grid:copy([[666]])))},
     {"Grid 1 x 2", ?_assertEqual([666, 777], grid:to_list(grid:copy([[666, 777]])))},
     {"Grid 2 x 1", ?_assertEqual([666, 777], grid:to_list(grid:copy([[666], [777]])))},
     {"Grid 2 x 2", ?_assertEqual([666, 777, 888, 999], grid:to_list(grid:copy([[666, 777], [888, 999]])))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_grid(RowCount :: pos_integer(), ColumnCount :: pos_integer(), Data :: [integer()], Grid :: grid:grid(integer())) -> 'ok'.
check_grid(RowCount, ColumnCount, Data, Grid) ->
    ?assertEqual(RowCount, grid:get_row_count(Grid)),
    ?assertEqual(ColumnCount, grid:get_column_count(Grid)),
    ?assertEqual(Data, grid:to_list(Grid)).