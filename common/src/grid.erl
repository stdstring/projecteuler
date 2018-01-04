%% @author std-string

-module(grid).
-export([create/3, copy/1, copy/3, get_row_count/1, get_column_count/1, get_value/2, get_value/3, set_value/3, set_value/4]).

-type grid_element_type() :: term(). %% interface ??

-type grid_type() :: array:array(grid_element_type()). %% impl
-record(grid, {row_count :: pos_integer(), column_count :: pos_integer(), grid :: grid_type()}). %% impl

-type row_type()::pos_integer(). %% interface
-type column_type()::pos_integer(). %% interface
-type point_type() :: {Row :: row_type(), Column :: column_type()}. %% interface
-type grid() :: #grid{}. %% interface

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(RowCount :: pos_integer(), ColumnCount :: pos_integer(), Value :: grid_element_type()) -> grid().
create(RowCount, ColumnCount, _Value) when not is_integer(RowCount); RowCount =< 0; not is_integer(ColumnCount); ColumnCount =< 0 -> error(badarg);
create(RowCount, ColumnCount, Value) ->
    Grid = array:new([{size, RowCount * ColumnCount}, {fixed, true}, {default, Value}]),
    #grid{row_count = RowCount, column_count = ColumnCount, grid = Grid}.

-spec copy(Data :: [[grid_element_type()]]) -> ok.
copy(Data) when not is_list(Data) -> error(badarg);
copy([]) -> error(badarg);
copy([Head | _Tail]) when not is_list(Head) -> error(badarg);
copy([Head | _Tail] = Data) ->
    RowCount = length(Data),
    ColumnCount = length(Head),
    %% TODO (std_string) : think about this check
    case lists:all(fun(Row) -> is_list(Row) andalso length(Row) == ColumnCount end, Data) of
        true -> copy(RowCount, ColumnCount, lists:flatten(Data));
        false -> error(badarg)
    end.

-spec copy(RowCount :: pos_integer(), ColumnCount :: pos_integer(), Data :: [grid_element_type()]) -> grid().
copy(RowCount, ColumnCount, _Data) when not is_integer(RowCount); RowCount =< 0; not is_integer(ColumnCount); ColumnCount =< 0 -> error(badarg);
copy(RowCount, ColumnCount, Data) when not is_list(Data); length(Data) /= (RowCount * ColumnCount) -> error(badarg);
copy(RowCount, ColumnCount, Data) -> #grid{row_count = RowCount, column_count = ColumnCount, grid = array:fix(array:from_list(Data))}.

-spec get_row_count(Grid :: grid()) -> pos_integer().
get_row_count(Grid) when not is_record(Grid, grid) -> error(badarg);
get_row_count(Grid) -> Grid#grid.row_count.

-spec get_column_count(Grid :: grid()) -> pos_integer().
get_column_count(Grid) when not is_record(Grid, grid) -> error(badarg);
get_column_count(Grid) -> Grid#grid.column_count.

-spec get_value(Point :: point_type(), Grid :: grid()) -> grid_element_type().
get_value({Row, Column}, Grid) -> get_value(Row, Column, Grid).

-spec get_value(Row :: row_type(), Column :: column_type(), Grid :: grid()) -> grid_element_type().
get_value(Row, Column, _Grid) when not is_integer(Row); Row =< 0; not is_integer(Column); Column =< 0 -> error(badarg);
get_value(_Row, _Column, Grid) when not is_record(Grid, grid) -> error(badarg);
get_value(Row, Column, Grid) when Row > Grid#grid.row_count; Column > Grid#grid.column_count -> error(badarg);
get_value(Row, Column, Grid) -> array:get(get_index(Row, Column, Grid), Grid#grid.grid).

-spec set_value(Point :: point_type(), Value :: grid_element_type(), Grid :: grid()) -> grid().
set_value({Row, Column}, Value, Grid) -> set_value(Row, Column, Value, Grid).

-spec set_value(Row :: row_type(), Column :: column_type(), Value :: grid_element_type(), Grid :: grid()) -> grid().
set_value(Row, Column, _Value, _Grid) when not is_integer(Row); Row =< 0; not is_integer(Column); Column =< 0 -> error(badarg);
set_value(_Row, _Column, _Value, Grid) when not is_record(Grid, grid) -> error(badarg);
set_value(Row, Column, _Value, Grid) when Row > Grid#grid.row_count; Column > Grid#grid.column_count -> error(badarg);
set_value(Row, Column, Value, Grid) -> Grid#grid{grid = array:set(get_index(Row, Column, Grid), Value, Grid#grid.grid)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_index(Row :: row_type(), Column :: column_type(), Grid :: grid()) -> non_neg_integer().
get_index(Row, Column, Grid) -> (Row - 1) * Grid#grid.column_count + (Column - 1).