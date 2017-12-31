%% @author std-string

%% What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20*20 grid (situated in "problem_011.dat")?

-module(problem_011).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-include("grid_def.hrl").

-type direction() :: 'horizontal' | 'vertical' | 'direct_diagonal' | 'reverse_diagonal'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{"problem_011.dat", 4}, 70600674}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, {Filename, TermCount}) ->
    Grid = load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), " "),
    {Grid, TermCount}.

-spec solve(PreparedInput :: term()) -> term().
solve({GridData, TermCount}) -> traverse_grid(grid_helper:create(GridData), TermCount).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec traverse_grid(Grid :: grid_type(), TermCount :: pos_integer()) -> pos_integer().
traverse_grid(Grid, TermCount) -> traverse_grid(1, 1, Grid, TermCount, 0).

-spec traverse_grid(Row :: pos_integer(),
                    Column :: pos_integer(),
                    Grid :: grid_type(),
                    TermCount :: pos_integer(),
                    MaxValue :: pos_integer()) -> pos_integer().
traverse_grid(Row, Column, Grid, TermCount, MaxValue) ->
    RowCount = grid_helper:get_row_count(Grid),
    ColumnCount = grid_helper:get_column_count(Grid),
    if
        Row > RowCount -> MaxValue;
        Column > ColumnCount -> traverse_grid(Row + 1, 1, Grid, TermCount, MaxValue);
        true ->
            HProduct = calc_direction_product(Row, Column, Grid, horizontal, TermCount),
            VProduct = calc_direction_product(Row, Column, Grid, vertical, TermCount),
            DProduct = calc_direction_product(Row, Column, Grid, direct_diagonal, TermCount),
            RProduct = calc_direction_product(Row, Column, Grid, reverse_diagonal, TermCount),
            NewMaxValue = lists:max([MaxValue, HProduct, VProduct, DProduct, RProduct]),
            traverse_grid(Row, Column + 1, Grid, TermCount, NewMaxValue)
    end.

-spec calc_direction_product(Row :: pos_integer(),
                             Column :: pos_integer(),
                             Grid :: grid_type(),
                             Direction :: direction(),
                             TermCount :: pos_integer()) -> non_neg_integer().
calc_direction_product(Row, Column, Grid, horizontal, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_cell_value(Row, Column + Index, Grid) * Acc end);
calc_direction_product(Row, Column, Grid, vertical, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_cell_value(Row + Index, Column, Grid) * Acc end);
calc_direction_product(Row, Column, Grid, direct_diagonal, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_cell_value(Row + Index, Column + Index, Grid) * Acc end);
calc_direction_product(Row, Column, Grid, reverse_diagonal, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_cell_value(Row + Index, Column - Index, Grid) * Acc end).

-spec get_cell_value(Row :: pos_integer(), Column :: pos_integer(), Grid :: grid_type()) -> integer().
get_cell_value(Row, Column, Grid) ->
    RowCount = grid_helper:get_row_count(Grid),
    ColumnCount = grid_helper:get_column_count(Grid),
    if
        Row > RowCount; Column > ColumnCount; Row < 1; Column < 1 -> 0;
        true -> grid_helper:get_value(Row, Column, Grid)
    end.