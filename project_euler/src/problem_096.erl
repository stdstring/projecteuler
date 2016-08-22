%% 

-module(problem_096).
-export([get_check_data/0, prepare_data/2, solve/1]).
-export([get_element/3, set_element/4, occupy_digit/2]).
-export([generate_row/1, generate_column/1, generate_square/2]).
-export([scan_row/2, scan_column/2, scan_square/3]).
-export([append_row_digits/3, append_column_digits/3, append_square_digits/4]).
-export([merge_cells_info/1]).
-export([choose_cell/1, strikeout_cell/4, check_digits_info/1]).
-export([process_row/2, process_column/2, process_square/3]).
-export([process_calculation/1]).
-export([create_context/1]).
-export([check_grid/1, get_row/2, get_column/2, get_square/3]).
-export([find_predict_object/1, create_predict_context/1, select_next_combination/1, create_prediction/2, apply_prediction/3]).

-behaviour(numerical_task_behaviour).

-define(SQUARE_SIDE, 3).
-define(GRID_SIDE, 9).
-define(ALL_NUMBERS ,2#111111111).

-record(grid_case, {name :: string(), grid :: array:array(integer())}).

-record(cell_info, {row :: 1 .. ?GRID_SIDE, column :: 1 .. ?GRID_SIDE, constraint :: integer()}).
-record(calc_context, {empty_count :: integer(), grid :: array:array(integer())}).
-record(predict_context, {cells :: [#cell_info{}], digits :: [1 .. ?GRID_SIDE], lex_number :: integer(), sup_lex_number :: integer(), grid :: array:array(integer())}).

get_check_data() ->
    [{"problem_096.dat", none}].

prepare_data(ModuleSourceDir, Filename) ->
    Strings = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    convert_data(Strings, []).

solve(Data) ->
    %%lists:foreach(fun(#grid_case{name = Name, grid = Grid}) -> solve_case(Name, Grid) end, Data).
    solve_impl(Data).

solve_impl([]) -> ok;
solve_impl([#grid_case{name = Name, grid = Grid} | Rest]) ->
    solve_case(Name, Grid),
    solve_impl(Rest).

solve_case(Name, InitGrid)->
    io:format("solve ~p~n", [Name]),
    io:format("grid before:~n", []),
    show_grid(InitGrid),
    SolvedGrid = process_grid([], create_context(InitGrid)),
    io:format("solved~n", []),
    case check_grid(SolvedGrid) of
        true -> io:format("solution checked~n", []);
        false ->
            io:format("bad_solution~n", []),
            error(bad_solution)
    end,
    io:format("grid after:~n", []),
    show_grid(SolvedGrid),
    io:format("~n", []).

process_grid(PredictionStack, CalcContextBefore) ->
    case process_calculation(CalcContextBefore) of
        stop_calc ->
            {DigitCombination, PredictContext, UpdatedPredictionStack} = create_prediction(PredictionStack, stop_calc),
            UpdatedGrid = apply_prediction(DigitCombination, PredictContext, PredictContext#predict_context.grid),
            process_grid(UpdatedPredictionStack, create_context(UpdatedGrid));
        {false, CalcContextAfter} ->
            {DigitCombination, PredictContext, UpdatedPredictionStack} = create_prediction(PredictionStack, CalcContextAfter),
            UpdatedGrid = apply_prediction(DigitCombination, PredictContext, CalcContextAfter#calc_context.grid),
            process_grid(UpdatedPredictionStack, create_context(UpdatedGrid));
        {true, CalcContextAfter} -> CalcContextAfter#calc_context.grid
    end.

%%solve_case(Name, Grid)->
%%    io:format("solve ~p~n", [Name]),
%%    io:format("grid before:~n", []),
%%    show_grid(Grid),
%%    ContextBefore = create_context(Grid),
%%    process_calculation_result(process_calculation(ContextBefore)).

%%process_calculation_result(stop_calc) -> throw(bad_solution);
%%process_calculation_result({true, Context}) ->
%%    io:format("solved~n", []),
%%    case check_grid(Context#calc_context.grid) of
%%        true -> io:format("solution checked~n", []);
%%        false -> throw(bad_solution)
%%    end,
%%    show_grid_after(Context#calc_context.grid);
%%process_calculation_result({false, Context}) ->
%%    io:format("can't be solved~n", []),
%%    show_grid_after(Context#calc_context.grid).

%%show_grid_after(Grid) ->
%%    io:format("grid after:~n", []),
%%    show_grid(Grid),
%%    io:format("~n", []).

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

merge_cells_info(CellsInfo) ->
    DestInit = array:new([{size, ?GRID_SIDE}, {fixed, true}, {default, undefined}]),
    lists:foldl(fun(CellInfo, Dest) -> merge_cell_info(CellInfo, Dest) end, DestInit, CellsInfo).

%% #cell_info{}, array:array([{X, Y}] | undefined) -> array:array([{X, Y}] | undefined)
merge_cell_info(#cell_info{row = Row, column = Column, constraint = DigitsBinary}, MergedInfo) ->
    DigitsList = get_free_digits_list(DigitsBinary),
    FoldlFun = fun(Digit, Dest) ->
        case array:get(Digit - 1, Dest) of
            undefined -> array:set(Digit - 1, [{Row, Column}], Dest);
            Cells -> array:set(Digit - 1, [{Row, Column}] ++ Cells, Dest)
        end
    end,
    lists:foldl(FoldlFun, MergedInfo, DigitsList).

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
strikeout_cell(Row, Column, Digit, DigitsInfo) ->
    MapFun = fun(_Index, undefined) -> undefined;
                (_Index, Cells) -> lists:delete({Row, Column}, Cells) end,
    array:map(MapFun, array:set(Digit - 1, undefined, DigitsInfo)).

%% array:array([{Row, Column}] | undefined) -> bool()
check_digits_info(DigitsInfo) ->
    FoldlFun = fun(_Index, [], _Result) -> false;
                  (_Index, _Value, Result) -> (Result and true) end,
    array:foldl(FoldlFun, true, DigitsInfo).

%% Row, #calc_context{} | stop_calc -> #calc_context{} | stop_calc
process_row(_SourceRow, stop_calc) -> stop_calc;
process_row(SourceRow, Context) ->
    Grid = Context#calc_context.grid,
    {FreeCells, InitCellDigits} = scan_row(SourceRow, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{row = Row, column = Column, constraint = append_square_digits(Row, Column, Grid, append_column_digits(Column, Grid, InitCellDigits))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

%% Column, #calc_context{} | stop_calc -> #calc_context{} | stop_calc
process_column(_SourceColumn, stop_calc) -> stop_calc;
process_column(SourceColumn, Context) ->
    Grid = Context#calc_context.grid,
    {FreeCells, InitCellDigits} = scan_column(SourceColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{row = Row, column = Column, constraint = append_square_digits(Row, Column, Grid, append_row_digits(Row, Grid, InitCellDigits))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

%% Row, Column, #calc_context{} | stop_calc -> #calc_context{} | stop_calc
process_square(_CellRow, _CellColumn, stop_calc) -> stop_calc;
process_square(CellRow, CellColumn, Context) ->
    Grid = Context#calc_context.grid,
    {FreeCells, InitCellDigits} = scan_square(CellRow, CellColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{row = Row, column = Column, constraint = append_column_digits(Column, Grid, append_row_digits(Row, Grid, InitCellDigits))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

%% [#cell_info{}], #calc_context{} | stop_calc -> #calc_context{} | stop_calc
process_cells(_CellsInfo, stop_calc) -> stop_calc;
process_cells(CellsInfo, Context) ->
    DigitsInfo = merge_cells_info(CellsInfo),
    case check_digits_info(DigitsInfo) of
        true -> process_digits(DigitsInfo, Context);
        false -> stop_calc
    end.

%% array:array([{Row, Column}]), #calc_context{} | stop_calc -> #calc_context{} | stop_calc
process_digits(_DigitsInfo, stop_calc) -> stop_calc;
process_digits(DigitsInfo, Context) ->
    case choose_cell(DigitsInfo) of
        {true, Digit, {Row, Column}} ->
            EmptyCount = Context#calc_context.empty_count,
            UpdatedGrid = set_element(Row, Column, Digit, Context#calc_context.grid),
            UpdatedContext = #calc_context{empty_count = EmptyCount - 1, grid = UpdatedGrid},
            UpdatedDigitsInfo = strikeout_cell(Row, Column, Digit, DigitsInfo),
            case check_digits_info(UpdatedDigitsInfo) of
                true -> process_digits(UpdatedDigitsInfo, UpdatedContext);
                false -> stop_calc
            end;
        false -> Context
    end.

%% #calc_context{} | stop_calc -> {bool(), #calc_context{}} | stop_calc
process_calculation(stop_calc) -> stop_calc;
process_calculation(Context) when Context#calc_context.empty_count == 0 -> {true, Context};
process_calculation(ContextBefore) ->
    ContextAfterRows = lists:foldl(fun(Row, Context) -> process_row(Row, Context) end, ContextBefore, lists:seq(1, ?GRID_SIDE)),
    ContextAfterColumns = lists:foldl(fun(Column, Context) -> process_column(Column, Context) end, ContextAfterRows, lists:seq(1, ?GRID_SIDE)),
    %% TODO (std_string) : think about generation
    Squares = [{1, 1}, {1, 4}, {1, 7}, {4, 1}, {4, 4}, {4, 7}, {7, 1}, {7, 4}, {7, 7}],
    ContextAfter = lists:foldl(fun({CellRow, CellColumn}, Context) -> process_square(CellRow, CellColumn, Context) end, ContextAfterColumns, Squares),
    if
        ContextAfter == stop_calc -> stop_calc;
        ContextAfter#calc_context.empty_count < ContextBefore#calc_context.empty_count -> process_calculation(ContextAfter);
        ContextAfter#calc_context.empty_count == ContextBefore#calc_context.empty_count -> {false, ContextAfter}
    end.

%% array:array(integer()) -> #calc_context{}
create_context(Grid) ->
    EmptyCount = array:foldl(fun(_Index, Value, Result) ->
        if
            Value == 0 -> Result + 1;
            Value > 0 -> Result
        end
    end, 0, Grid),
    #calc_context{empty_count = EmptyCount, grid = Grid}.

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

%% array:array(integer()) -> FoundObject
find_predict_object(Grid) ->
    %%io:format("find_predict_object(Grid)~n", []),
    FoundRow = lists:foldl(fun(Row, Result) -> merge_predict_object(Result, scan_row(Row, Grid)) end, {[], 0}, lists:seq(1, ?GRID_SIDE)),
    FoundColumn = lists:foldl(fun(Column, Result) -> merge_predict_object(Result, scan_column(Column, Grid)) end, FoundRow, lists:seq(1, ?GRID_SIDE)),
    %% TODO (std_string) : think about generation
    Squares = [{1, 1}, {1, 4}, {1, 7}, {4, 1}, {4, 4}, {4, 7}, {7, 1}, {7, 4}, {7, 7}],
    lists:foldl(fun({Row, Column}, Result) -> merge_predict_object(Result, scan_square(Row, Column, Grid)) end, FoundColumn, Squares).

%% OldFoundObject, NewFoundObject -> FoundObject
merge_predict_object({[], 0}, {[], 0}) -> {[], 0};
merge_predict_object({[], 0}, NewFoundObject) -> NewFoundObject;
merge_predict_object(OldFoundObject, {[], 0}) -> OldFoundObject;
merge_predict_object({OldCells, OldDigits}, {NewCells, _NewDigits}) when length(OldCells) =< length(NewCells) -> {OldCells, OldDigits};
merge_predict_object(_OldFoundObject, NewFoundObject) -> NewFoundObject.

%% array:array(integer()) -> #predict_context{}
create_predict_context(Grid) ->
    %%io:format("create_predict_context(Grid)~n", []),
    {Cells, DigitsBinary} = find_predict_object(Grid),
    create_predict_context(Cells, DigitsBinary, Grid).

%% {Row, Column}, DigitsBinary, array:array(integer()) -> #predict_context{}
create_predict_context(Cells, DigitsBinary, Grid) ->
    %%io:format("create_predict_context(Cells, DigitsBinary, Grid)~n", []),
    Digits = get_free_digits_list(DigitsBinary),
    CellsInfo = lists:map(fun({Row, Column}) ->
        Constraint = append_square_digits(Row, Column, Grid, append_column_digits(Column, Grid, append_row_digits(Row, Grid, DigitsBinary))),
        #cell_info{row = Row, column = Column, constraint = Constraint}
    end, Cells),
    %% TODO (std_string) : move this into permutations module
    SupLexNumber = numbers:factorial(length(Digits)),
    io:format("cells = ~p, digits = ~p~n", [CellsInfo, Digits]),
    case length(Digits) of
        1 ->
            io:format("special show:~n", []),
            show_grid(Grid);
        _ -> ok
    end,
    #predict_context{cells = CellsInfo, digits = Digits, lex_number = -1, sup_lex_number = SupLexNumber - 1, grid = Grid}.

%% #predict_context{} -> {[1..9], #predict_context{}} | finish
select_next_combination(#predict_context{lex_number = Number, sup_lex_number = Number}) -> finish;
select_next_combination(Context) ->
    %%io:format("select_next_combination(Context)~n", []),
    %%io:format("select_next_combination(Context) point 1~n", []),
    NextLexNumber = Context#predict_context.lex_number + 1,
    %%io:format("select_next_combination(Context) point 2~n", []),
    %%NextDigitCombination = permutations:get_permutation(NextLexNumber, Context#predict_context.digits),
    %%io:format("NextLexNumber = ~p, Context#predict_context.digits = ~p~n", [NextLexNumber, Context#predict_context.digits]),
    NextDigitCombination = permutations:get_permutation(NextLexNumber, array:fix(array:from_list(Context#predict_context.digits))),
    %%io:format("select_next_combination(Context) point 3~n", []),
    UpdatedContext = Context#predict_context{lex_number = NextLexNumber},
    %%io:format("select_next_combination(Context) point 4~n", []),
    case check_digit_combination(Context#predict_context.cells, NextDigitCombination) of
        true -> {NextDigitCombination, UpdatedContext};
        false -> select_next_combination(UpdatedContext)
    end.

%% [#cell_info{}], [1..9] -> bool()
check_digit_combination([], []) -> true;
check_digit_combination([#cell_info{constraint = Constraint} | CellsInfoRest], [Digit | DigitsRest]) ->
    %%io:format("check_digit_combination([#cell_info{constraint = Constraint} | CellsInfoRest], [Digit | DigitsRest])~n", []),
    CheckDigit = Constraint band (1 bsl (Digit - 1)),
    if
        CheckDigit == 0 -> false;
        CheckDigit /= 0 -> check_digit_combination(CellsInfoRest, DigitsRest)
    end.

%% [#predict_context{}], #calc_context{} | stop_calc -> {[1..9], #predict_context{}, [#predict_context{}]} | no_return()
%% TODO (std_string) : add details to exception
create_prediction([], stop_calc) -> error(invalid_operation);
create_prediction([PredictContext | PredictionStackRest], stop_calc) ->
    %%io:format("create_prediction([PredictContext | PredictionStackRest], stop_calc)~n", []),
    case select_next_combination(PredictContext) of
        finish -> create_prediction(PredictionStackRest, stop_calc);
        {DigitCombination, UpdatedPredictContext} -> {DigitCombination, UpdatedPredictContext, [UpdatedPredictContext] ++ PredictionStackRest}
    end;
create_prediction(PredictionStack, CalcContext) ->
    %%io:format("create_prediction(PredictionStack, CalcContext)~n", []),
    {DigitCombination, PredictContext} = select_next_combination(create_predict_context(CalcContext#calc_context.grid)),
    {DigitCombination, PredictContext, [PredictContext] ++ PredictionStack}.

%% [1..9], #predict_context{}, array:array(integer()) -> array:array(integer())
apply_prediction(DigitCombination, PredictContext, Grid) -> apply_prediction_impl(DigitCombination, PredictContext#predict_context.cells, Grid).

%% [1..9], [#cell_info{}], array:array(integer()) -> array:array(integer())
apply_prediction_impl([], [], Grid) -> Grid;
apply_prediction_impl([Digit | DigitsRest], [#cell_info{row = Row, column = Column} | CellsInfoRest], Grid) ->
    apply_prediction_impl(DigitsRest, CellsInfoRest, set_element(Row, Column, Digit, Grid)).