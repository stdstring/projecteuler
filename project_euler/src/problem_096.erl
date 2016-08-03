%% 

-module(problem_096).
-export([get_check_data/0, prepare_data/2, solve/1]).
-export([get_element/3, set_element/4, occupy_digit/2]).
-export([generate_row/2, generate_column/2, generate_square/2]).
-export([scan_row/2, scan_column/2, scan_square/3]).
-export([append_row_digits/4, append_column_digits/4, append_square_digits/4]).
-export([merge_cells_info/1]).

-behaviour(numerical_task_behaviour).

-define(SQUARE_SIDE, 3).
-define(GRID_SIDE, 9).
-define(ALL_NUMBERS ,2#111111111).

-record(grid_case, {name :: string(), grid :: array:array(integer())}).

get_check_data() ->
    [{"problem_096.dat", none}].

prepare_data(ModuleSourceDir, Filename) ->
    Strings = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    convert_data(Strings, []).

solve(_Data) ->
    ok.

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
    Grid = array:from_list(Row1 ++ Row2 ++ Row3 ++ Row4 ++ Row5 ++ Row6 ++ Row7 ++ Row8 ++ Row9),
    convert_data(Rest, [#grid_case{name = Description, grid = Grid}] ++ Dest).

%% TODO (std_string) : use such approach in all other cases
get_element(X, Y, Grid) ->
    Index = (Y - 1) * ?GRID_SIDE + (X - 1),
    array:get(Index, Grid).

set_element(X, Y, Value, Grid) ->
    Index = (Y - 1) * ?GRID_SIDE + (X - 1),
    array:set(Index, Value, Grid).

occupy_digit(Digits, Digit) -> Digits band bnot(1 bsl (Digit - 1)).

%% return coordinates of row which contains the current cell
%% TODO (std_string) : think about generate_row/1 instead of generate_row/2
generate_row(_X, Y) -> lists:map(fun(X) -> {X, Y} end, lists:seq(1, ?GRID_SIDE)).

%% return coordinates of column which contains the current cell
%% TODO (std_string) : think about generate_column/1 instead of generate_column/2
generate_column(X, _Y) -> lists:map(fun(Y) -> {X, Y} end, lists:seq(1, ?GRID_SIDE)).

%% return coordinates of square which contains the current cell
generate_square(X, Y) ->
    XLeft = ?SQUARE_SIDE * (X div ?SQUARE_SIDE),
    YTop = ?SQUARE_SIDE * (Y div ?SQUARE_SIDE),
    %%  TODO (std_string) : probably use more smart approach
    [{XLeft, YTop}, {XLeft + 1, YTop}, {XLeft + 2, YTop}, {XLeft, YTop + 1}, {XLeft + 1, YTop + 1}, {XLeft + 2, YTop + 1}, {XLeft, YTop + 2}, {XLeft + 1, YTop + 2}, {XLeft + 2, YTop + 2}].

scan_row(RowY, Grid) ->
    scan_cells(Grid, generate_row(-1, RowY)).

scan_column(ColumnX, Grid) ->
    scan_cells(Grid, generate_column(ColumnX, -1)).

scan_square(CellX, CellY, Grid) ->
    scan_cells(Grid, generate_square(CellX, CellY)).

scan_cells(Grid, Cells) ->
    lists:foldl(fun ({X, Y}, {FreeCells, Digits}) ->
        CellValue = get_element(X, Y, Grid),
        if
            CellValue == 0 -> {[{X, Y}] ++ FreeCells, Digits};
            CellValue /= 0 -> {FreeCells, occupy_digit(Digits, CellValue)}
        end
    end, {[], ?ALL_NUMBERS}, Cells).

%% TODO (std_string) : think about append_row_digits/3 instead of append_row_digits/4
append_row_digits(_CellX, CellY, Grid, CellDigits) ->
    append_cells_digits(Grid, CellDigits, generate_row(-1, CellY)).

%% TODO (std_string) : think about append_column_digits/3 instead of append_column_digits/4
append_column_digits(CellX, _CellY, Grid, CellDigits) ->
    append_cells_digits(Grid, CellDigits, generate_column(CellX, -1)).

append_square_digits(CellX, CellY, Grid, CellDigits) ->
    append_cells_digits(Grid, CellDigits, generate_square(CellX, CellY)).

append_cells_digits(Grid, SourceDigits, Cells) ->
    lists:foldl(fun ({X, Y}, Digits) ->
        CellValue = get_element(X, Y, Grid),
        if
            CellValue == 0 -> Digits;
            CellValue /= 0 -> occupy_digit(Digits, CellValue)
        end
    end, SourceDigits, Cells).

%% CellsInfo = [{X, Y, DigitsBinray}]
merge_cells_info(CellsInfo) ->
    DestInit = array:new([{size, ?GRID_SIDE}, {fixed,true}, {default, []}]),
    lists:foldl(fun({X, Y, DigitsBinary}, Dest) -> merge_cell_info(X, Y , DigitsBinary, Dest) end, DestInit, CellsInfo).

merge_cell_info(X, Y, DigitsBinary, MergedInfo) ->
    DigitsList = get_free_digits_list(DigitsBinary),
    lists:foldl(fun(Digit, Dest) -> array:set(Digit - 1, [{X, Y}] ++ array:get(Digit - 1, Dest), Dest) end, MergedInfo, DigitsList).

get_free_digits_list(DigitsBinary) ->
    lists:foldl(fun(Digit, DigitsList) ->
        case DigitsBinary band (1 bsl (Digit - 1)) of
            0 -> DigitsList;
            _ -> [Digit] ++ DigitsList
        end
    end, [], lists:seq(1, ?GRID_SIDE)).