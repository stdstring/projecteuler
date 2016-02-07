%% @author std-string

-module(grid_helper).
-export([create/1, create/3, get_row_count/1, get_column_count/1, get_value/3, set_value/4]).

-include("grid_def.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(GridData :: [[term()]]) -> grid_type().
create(GridData) -> fill(GridData, 0, array:new()).

-spec create(RowCount :: pos_integer(), ColumnCount :: pos_integer(), Value :: term()) -> grid_type().
create(RowCount, ColumnCount, Value) ->
    InitGrid = array:new([{size, RowCount}, {fixed, true}]),
    control:for(RowCount, InitGrid, fun(Index, Grid) -> array:set(Index, array:new([{size, ColumnCount}, {fixed, true}, {default, Value}]), Grid) end).

-spec get_row_count(Grid :: grid_type()) -> pos_integer().
get_row_count(Grid) -> array:size(Grid).

-spec get_column_count(Grid :: grid_type()) -> pos_integer().
get_column_count(Grid) -> array:size(array:get(0, Grid)).

-spec get_value(Row :: pos_integer(), Column :: pos_integer(), Grid :: grid_type()) -> term().
get_value(Row, Column, Grid) -> array:get(Column - 1, array:get(Row - 1, Grid)).

-spec set_value(Row :: pos_integer(), Column :: pos_integer(), Value :: term(), Grid :: grid_type()) -> grid_type().
set_value(Row, Column, Value, Grid) ->
    GridRow = array:get(Row - 1, Grid),
    NewGridRow = array:set(Column - 1, Value, GridRow),
    array:set(Row - 1, NewGridRow, Grid).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec fill(GridData :: [[term()]], Index :: non_neg_integer(), Grid :: grid_type()) -> grid_type().
fill([], _Index, Grid) -> array:fix(Grid);
fill([DataRow | DataRest], Index, Grid) ->
    Row = array:fix(array:from_list(DataRow)),
    fill(DataRest, Index + 1, array:set(Index, Row, Grid)).