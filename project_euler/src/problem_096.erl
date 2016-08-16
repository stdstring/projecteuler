%% 

-module(problem_096).
-export([get_check_data/0, prepare_data/2, solve/1]).
-export([get_element/3, set_element/4, occupy_digit/2]).
-export([generate_row/1, generate_column/1, generate_square/2]).
-export([scan_row/2, scan_column/2, scan_square/3]).
-export([append_row_digits/3, append_column_digits/3, append_square_digits/4]).
-export([merge_cells_info/1]).
-export([choose_cell/1, strikeout_cell/3]).
-export([process_row/2, process_column/2, process_square/3]).
-export([process_calculation/1]).
-export([create_context/1]).
-export([check_grid/1, get_row/2, get_column/2, get_square/3]).

-behaviour(numerical_task_behaviour).

-define(SQUARE_SIDE, 3).
-define(GRID_SIDE, 9).
-define(ALL_NUMBERS ,2#111111111).

-record(grid_case, {name :: string(), grid :: array:array(integer())}).

-record(context, {empty_count :: integer(), grid :: array:array(integer())}).

get_check_data() ->
    [{"problem_096.dat", none}].

prepare_data(ModuleSourceDir, Filename) ->
    Strings = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    convert_data(Strings, []).

solve(Data) ->
    lists:foreach(fun(#grid_case{name = Name, grid = Grid}) -> solve_case(Name, Grid) end, Data).

solve_case(Name, Grid)->
    io:format("solve ~p~n", [Name]),
    io:format("grid before:~n", []),
    show_grid(Grid),
    ContextBefore = create_context(Grid),
    {Result, ContextAfter} = process_calculation(ContextBefore),
    if
        Result == true ->
            io:format("solved~n", []),
            case check_grid(ContextAfter#context.grid) of
                true -> io:format("solution checked~n", []);
                false -> throw(bad_solution)
            end;
        Result == false -> io:format("can't be solved~n", [])
    end,
    io:format("grid after:~n", []),
    show_grid(ContextAfter#context.grid),
    io:format("~n", []).

show_grid(Grid) ->
    ShowRowFun= fun(Row)->
        lists:foreach(fun(Column) -> io:format("~p ", [get_element(Row, Column, Grid)]) end, lists:seq(1, ?GRID_SIDE)),
        io:format("~n", [])
    end,
    lists:foreach(ShowRowFun, lists:seq(1, ?GRID_SIDE)).

convert_data([], Dest) -> lists:reverse(Dest);
convert_data([Description, Str1, Str2, Str3, Str4, Str5, Str6, Str7, Str8, Str9 | Rest], Dest) ->
    Row1 = lists:map(fun(Char) -> Char - $0 end, Str1),
    Row2 = lists:map(fun(Char) -> Char - $0 end, Str2),
    Row3 = lists:map(fun(Char) -> Char - $0 end, Str3),
    Row4 = lists:map(fun(Char) -> Char - $0 end, Str4),
    Row5 = lists:map(fun(Char) -> Char - $0 end, Str5),
    Row6 = lists:map(fun(Char) -> Char - $0 end, Str6),
    Row7 = lists:map(fun(Char) -> Char - $0 end, Str7),
    Row8 = lists:map(fun(Char) -> Char - $0 end, Str8),
    Row9 = lists:map(fun(Char) -> Char - $0 end, Str9),
    Grid = array:fix(array:from_list(Row1 ++ Row2 ++ Row3 ++ Row4 ++ Row5 ++ Row6 ++ Row7 ++ Row8 ++ Row9)),
    convert_data(Rest, [#grid_case{name = Description, grid = Grid}] ++ Dest).

%% TODO (std_string) : use such approach in all other cases
get_element(Row, Column, Grid) ->
    Index = (Row - 1) * ?GRID_SIDE + (Column - 1),
    array:get(Index, Grid).

%% TODO (std_string) : use such approach in all other cases
set_element(Row, Column, Value, Grid) ->
    Index = (Row - 1) * ?GRID_SIDE + (Column - 1),
    array:set(Index, Value, Grid).

occupy_digit(Digits, Digit) -> Digits band bnot(1 bsl (Digit - 1)).

%% return coordinates of row which contains the current cell
generate_row(Row) -> lists:map(fun(Column) -> {Row, Column} end, lists:seq(1, ?GRID_SIDE)).

%% return coordinates of column which contains the current cell
generate_column(Column) -> lists:map(fun(Row) -> {Row, Column} end, lists:seq(1, ?GRID_SIDE)).

%% return coordinates of square which contains the current cell
generate_square(Row, Column) ->
    RowTop = ?SQUARE_SIDE * ((Row - 1) div ?SQUARE_SIDE) + 1,
    ColumnLeft = ?SQUARE_SIDE * ((Column - 1) div ?SQUARE_SIDE) + 1,
    %%  TODO (std_string) : probably use more smart approach
    [{RowTop, ColumnLeft},
     {RowTop, ColumnLeft + 1},
     {RowTop, ColumnLeft + 2},
     {RowTop + 1, ColumnLeft},
     {RowTop + 1, ColumnLeft + 1},
     {RowTop + 1, ColumnLeft + 2},
     {RowTop + 2, ColumnLeft},
     {RowTop + 2, ColumnLeft + 1},
     {RowTop + 2, ColumnLeft + 2}].

scan_row(Row, Grid) -> scan_cells(Grid, generate_row(Row)).

scan_column(Column, Grid) -> scan_cells(Grid, generate_column(Column)).

scan_square(CellRow, CellColumn, Grid) -> scan_cells(Grid, generate_square(CellRow, CellColumn)).

scan_cells(Grid, Cells) ->
    lists:foldl(fun ({Row, Column}, {FreeCells, Digits}) ->
        CellValue = get_element(Row, Column, Grid),
        if
            CellValue == 0 -> {[{Row, Column}] ++ FreeCells, Digits};
            CellValue /= 0 -> {FreeCells, occupy_digit(Digits, CellValue)}
        end
    end, {[], ?ALL_NUMBERS}, Cells).

append_row_digits(CellRow, Grid, CellDigits) -> append_cells_digits(Grid, CellDigits, generate_row(CellRow)).

append_column_digits(CellColumn, Grid, CellDigits) -> append_cells_digits(Grid, CellDigits, generate_column(CellColumn)).

append_square_digits(CellRow, CellColumn, Grid, CellDigits) -> append_cells_digits(Grid, CellDigits, generate_square(CellRow, CellColumn)).

append_cells_digits(Grid, SourceDigits, Cells) ->
    lists:foldl(fun ({Row, Column}, Digits) ->
        CellValue = get_element(Row, Column, Grid),
        if
            CellValue == 0 -> Digits;
            CellValue /= 0 -> occupy_digit(Digits, CellValue)
        end
    end, SourceDigits, Cells).

%% CellsInfo = [{Row, Column, DigitsBinray}]
merge_cells_info(CellsInfo) ->
    DestInit = array:new([{size, ?GRID_SIDE}, {fixed, true}, {default, []}]),
    lists:foldl(fun({Row, Column, DigitsBinary}, Dest) -> merge_cell_info(Row, Column, DigitsBinary, Dest) end, DestInit, CellsInfo).

merge_cell_info(Row, Column, DigitsBinary, MergedInfo) ->
    DigitsList = get_free_digits_list(DigitsBinary),
    lists:foldl(fun(Digit, Dest) -> array:set(Digit - 1, [{Row, Column}] ++ array:get(Digit - 1, Dest), Dest) end, MergedInfo, DigitsList).

get_free_digits_list(DigitsBinary) ->
    lists:foldl(fun(Digit, DigitsList) ->
        case DigitsBinary band (1 bsl (Digit - 1)) of
            0 -> DigitsList;
            _ -> [Digit] ++ DigitsList
        end
    end, [], lists:seq(1, ?GRID_SIDE)).

%% ... -> {true, Digit, {Row, Column}} | false
choose_cell(DigitsInfo) -> choose_cell(DigitsInfo, 0).

%% ... -> {true, Digit, {Row, Column}} | false
choose_cell(_DigitsInfo, ?GRID_SIDE) -> false;
choose_cell(DigitsInfo, Index) ->
    case array:get(Index, DigitsInfo) of
        [{Row, Column}] -> {true, Index + 1, {Row, Column}};
        _Other -> choose_cell(DigitsInfo, Index + 1)
    end.

%% array:array([{Row, Column}]) -> array:array([{Row, Column}])
strikeout_cell(Row, Column, DigitsInfo) ->
    array:map(fun(_Index, Cells) -> lists:delete({Row, Column}, Cells) end, DigitsInfo).

%% Row, #context{} -> #context{}
process_row(SourceRow, Context) ->
    Grid = Context#context.grid,
    {FreeCells, InitCellDigits} = scan_row(SourceRow, Grid),
    CellsInfo = lists:map(fun({Row, Column}) -> {Row, Column, append_square_digits(Row, Column, Grid, append_column_digits(Column, Grid, InitCellDigits))} end, FreeCells),
    DigitsInfo = merge_cells_info(CellsInfo),
    process_cells(DigitsInfo, Context).

%% Column, #context{} -> #context{}
process_column(SourceColumn, Context) ->
    Grid = Context#context.grid,
    {FreeCells, InitCellDigits} = scan_column(SourceColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) -> {Row, Column, append_square_digits(Row, Column, Grid, append_row_digits(Row, Grid, InitCellDigits))} end, FreeCells),
    DigitsInfo = merge_cells_info(CellsInfo),
    process_cells(DigitsInfo, Context).

%% Row, Column, #context{} -> #context{}
process_square(CellRow, CellColumn, Context) ->
    Grid = Context#context.grid,
    {FreeCells, InitCellDigits} = scan_square(CellRow, CellColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) -> {Row, Column, append_column_digits(Column, Grid, append_row_digits(Row, Grid, InitCellDigits))} end, FreeCells),
    DigitsInfo = merge_cells_info(CellsInfo),
    process_cells(DigitsInfo, Context).

%% array:array([{Row, Column}]), #context{} -> #context{}
process_cells(DigitsInfo, Context) ->
    case choose_cell(DigitsInfo) of
        {true, Digit, {Row, Column}} ->
            EmptyCount = Context#context.empty_count,
            UpdatedGrid = set_element(Row, Column, Digit, Context#context.grid),
            UpdatedContext = #context{empty_count = EmptyCount - 1, grid = UpdatedGrid},
            process_cells(strikeout_cell(Row, Column, DigitsInfo), UpdatedContext);
        false -> Context
    end.

%% #context{} -> {bool(), #context{}}
process_calculation(Context) when Context#context.empty_count == 0 -> {true, Context};
process_calculation(ContextBefore) ->
    ContextAfterRows = lists:foldl(fun(Row, Context) -> process_row(Row, Context) end, ContextBefore, lists:seq(1, ?GRID_SIDE)),
    ContextAfterColumns = lists:foldl(fun(Column, Context) -> process_column(Column, Context) end, ContextAfterRows, lists:seq(1, ?GRID_SIDE)),
    %% TODO (std_string) : think about generation
    Squares = [{1, 1}, {1, 4}, {1, 7}, {4, 1}, {4, 4}, {4, 7}, {7, 1}, {7, 4}, {7, 7}],
    ContextAfter = lists:foldl(fun({CellRow, CellColumn}, Context) -> process_square(CellRow, CellColumn, Context) end, ContextAfterColumns, Squares),
    if
        ContextAfter#context.empty_count < ContextBefore#context.empty_count -> process_calculation(ContextAfter);
        ContextAfter#context.empty_count == ContextBefore#context.empty_count -> {false, ContextAfter}
    end.

%% array:array(integer()) -> #context{}
create_context(Grid) ->
    EmptyCount = array:foldl(fun(_Index, Value, Result) ->
        if
            Value == 0 -> Result + 1;
            Value > 0 -> Result
        end
    end, 0, Grid),
    #context{empty_count = EmptyCount, grid = Grid}.

%% array:array(integer()) -> bool()
check_grid(Grid) ->
    Expected = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    CheckRowResult = lists:foldl(fun(Row, Result) -> Result and (lists:sort(get_row(Grid, Row)) == Expected) end, true, lists:seq(1, ?GRID_SIDE)),
    CheckColumnResult = lists:foldl(fun(Column, Result) -> Result and (lists:sort(get_column(Grid, Column)) == Expected) end, CheckRowResult, lists:seq(1, ?GRID_SIDE)),
    %% TODO (std_string) : think about generation
    Squares = [{1, 1}, {1, 4}, {1, 7}, {4, 1}, {4, 4}, {4, 7}, {7, 1}, {7, 4}, {7, 7}],
    lists:foldl(fun({Row, Column}, Result) -> Result and (lists:sort(get_square(Grid, Row, Column)) == Expected) end, CheckColumnResult, Squares).

%% array:array(integer()), Row -> [integer()]
get_row(Grid, SourceRow) -> lists:map(fun({Row, Column}) -> get_element(Row, Column, Grid) end, generate_row(SourceRow)).

%% array:array(integer()), Column -> [integer()]
get_column(Grid, SourceColumn) -> lists:map(fun({Row, Column}) -> get_element(Row, Column, Grid) end, generate_column(SourceColumn)).

%% array:array(integer()), Row, Column -> [integer()]
get_square(Grid, RowTop, ColumnLeft) -> lists:map(fun({Row, Column}) -> get_element(Row, Column, Grid) end, generate_square(RowTop, ColumnLeft)).