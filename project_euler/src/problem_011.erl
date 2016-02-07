%% What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20*20 grid (situated in "problem_011.dat")?

-module(problem_011).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{{"problem_011.dat", 4}, 70600674}].

prepare_data(ModuleSourceDir, {Filename, TermCount}) ->
    Grid = load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), " "),
    {Grid, TermCount}.

solve({Grid, TermCount}) -> traverse_grid(Grid, TermCount).

traverse_grid(Grid, TermCount) -> traverse_grid(Grid, 1, 1, TermCount, 0).

traverse_grid(Grid, Row, 1, _, MaxValue) when Row > length(Grid) -> MaxValue;
traverse_grid(Grid, Row, Column, TermCount, MaxValue) when Column > length(Grid) -> traverse_grid(Grid, Row + 1, 1, TermCount, MaxValue);
traverse_grid(Grid, Row, Column, TermCount, MaxValue) ->
    HProduct = calc_direction_product(Grid, horizontal, Row, Column, TermCount),
    VProduct = calc_direction_product(Grid, vertical, Row, Column, TermCount),
    DProduct = calc_direction_product(Grid, direct_diagonal, Row, Column, TermCount),
    RProduct = calc_direction_product(Grid, reverse_diagonal, Row, Column, TermCount),
    NewMaxValue = lists:max([MaxValue, HProduct, VProduct, DProduct, RProduct]),
    traverse_grid(Grid, Row, Column + 1, TermCount, NewMaxValue).

calc_direction_product(Grid, horizontal, Row, Column, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_nth(Grid, Row, Column + Index) * Acc end);
calc_direction_product(Grid, vertical, Row, Column, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_nth(Grid, Row + Index, Column) * Acc end);
calc_direction_product(Grid, direct_diagonal, Row, Column, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_nth(Grid, Row + Index, Column + Index) * Acc end);
calc_direction_product(Grid, reverse_diagonal, Row, Column, TermCount) ->
    control:for(TermCount, 1, fun(Index, Acc) -> get_nth(Grid, Row + Index, Column - Index) * Acc end).

get_nth(Grid, Row, Column) when Row > length(Grid); Column > length(Grid); Row < 1; Column < 1 -> 0;
get_nth(Grid, Row, Column) -> lists:nth(Column, lists:nth(Row, Grid)).