%% @author std-string

%% Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept.
%% Its origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar, and much more difficult, puzzle idea called Latin Squares.
%% The objective of Su Doku puzzles, however, is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column, and 3 by 3 box contains each of the digits 1 to 9.
%% A well constructed Su Doku puzzle has a unique solution and can be solved by logic, although it may be necessary to employ "guess and test" methods in order to eliminate options (there is much contested opinion over this).
%% The complexity of the search determines the difficulty of the puzzle.
%% problem_096.dat is the data file, which contains fifty different Su Doku puzzles ranging in difficulty, but all with unique solutions.
%% By solving all fifty puzzles find the sum of the 3-digit numbers found in the top left corner of each solution grid.

-module(problem_096).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(SQUARE_SIDE, 3).
-define(GRID_SIDE, 9).
-define(ALL_NUMBERS ,2#111111111).
-define(DIGIT_1 ,2#000000001).
-define(DIGIT_2 ,2#000000010).
-define(DIGIT_3 ,2#000000100).
-define(DIGIT_4 ,2#000001000).
-define(DIGIT_5 ,2#000010000).
-define(DIGIT_6 ,2#000100000).
-define(DIGIT_7 ,2#001000000).
-define(DIGIT_8 ,2#010000000).
-define(DIGIT_9 ,2#100000000).
-define(SQUARES, [{1, 1}, {1, 4}, {1, 7}, {4, 1}, {4, 4}, {4, 7}, {7, 1}, {7, 4}, {7, 7}]).

%% TODO (std_string) : move into common and use such approach in all other cases
-type grid_type() :: array:array(integer()).
-type digit_type() :: 1..9.
-type row_type() :: 1..?GRID_SIDE.
-type column_type() :: 1..?GRID_SIDE.
-type coord_type() :: {Row :: row_type(), Column :: column_type()}.
-type found_type() :: {Cells :: [coord_type()], Digits :: non_neg_integer()}.
-type digits_info() :: array:array([coord_type()]).

-record(grid_case, {name :: string(), grid :: grid_type()}).

-record(cell_info, {row :: row_type(), column :: column_type(), constraint :: non_neg_integer()}).
-record(calc_context, {empty_count :: non_neg_integer(), grid :: grid_type()}).
-record(predict_context, {cells :: [#cell_info{}],
                          digits :: [digit_type()],
                          lex_number :: non_neg_integer(),
                          sup_lex_number :: non_neg_integer(),
                          grid :: grid_type()}).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{"problem_096.dat", 24702}].

prepare_data(ModuleSourceDir, Filename) ->
    Strings = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    convert_data(Strings, []).

solve(Data) ->
    lists:foldl(fun(#grid_case{name = Name, grid = Grid}, Sum) -> Sum + solve_case(Name, Grid) end, 0, Data).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec solve_case(Name :: string(), InitGrid :: grid_type())-> pos_integer().
solve_case(_Name, InitGrid)->
    Grid = process_grid([], create_context(InitGrid)),
    get_element(1, 1, Grid) * 100 + get_element(1, 2, Grid) * 10 + get_element(1, 3, Grid).

-spec process_grid(PredictionStack :: [#predict_context{}], CalcContextBefore :: #calc_context{}) -> grid_type().
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

-spec convert_data(Source :: [string()], Dest :: [#grid_case{}]) -> [#grid_case{}].
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

%% TODO (std_string) : move into common and use such approach in all other cases
-spec get_element(Row :: row_type(), Column :: column_type(), Grid :: grid_type()) -> integer().
get_element(Row, Column, Grid) ->
    Index = (Row - 1) * ?GRID_SIDE + (Column - 1),
    array:get(Index, Grid).

%% TODO (std_string) : move into common and use such approach in all other cases
-spec set_element(Row :: row_type(), Column :: column_type(), Value :: integer(), Grid :: grid_type()) -> grid_type().
set_element(Row, Column, Value, Grid) ->
    Index = (Row - 1) * ?GRID_SIDE + (Column - 1),
    array:set(Index, Value, Grid).

-spec occupy_digit(Digits :: non_neg_integer(), Digit :: digit_type()) -> non_neg_integer().
occupy_digit(Digits, Digit) -> Digits band bnot(1 bsl (Digit - 1)).

-spec generate_row(Row :: row_type()) -> [coord_type()].
generate_row(Row) -> lists:map(fun(Column) -> {Row, Column} end, lists:seq(1, ?GRID_SIDE)).

-spec generate_column(Column :: column_type()) -> [coord_type()].
generate_column(Column) -> lists:map(fun(Row) -> {Row, Column} end, lists:seq(1, ?GRID_SIDE)).

-spec generate_square(Row :: row_type(), Column :: column_type()) -> [coord_type()].
generate_square(Row, Column) ->
    RowTop = ?SQUARE_SIDE * ((Row - 1) div ?SQUARE_SIDE) + 1,
    ColumnLeft = ?SQUARE_SIDE * ((Column - 1) div ?SQUARE_SIDE) + 1,
    [{RowTop, ColumnLeft},
     {RowTop, ColumnLeft + 1},
     {RowTop, ColumnLeft + 2},
     {RowTop + 1, ColumnLeft},
     {RowTop + 1, ColumnLeft + 1},
     {RowTop + 1, ColumnLeft + 2},
     {RowTop + 2, ColumnLeft},
     {RowTop + 2, ColumnLeft + 1},
     {RowTop + 2, ColumnLeft + 2}].

-spec scan_row(Row :: row_type(), Grid :: grid_type()) -> found_type().
scan_row(Row, Grid) -> scan_cells(Grid, generate_row(Row)).

-spec scan_column(Column :: column_type(), Grid :: grid_type()) -> found_type().
scan_column(Column, Grid) -> scan_cells(Grid, generate_column(Column)).

-spec scan_square(CellRow :: row_type(), CellColumn :: column_type(), Grid :: grid_type()) -> found_type().
scan_square(CellRow, CellColumn, Grid) -> scan_cells(Grid, generate_square(CellRow, CellColumn)).

-spec scan_cells(Grid :: grid_type(), Cells :: [coord_type()]) -> found_type().
scan_cells(Grid, Cells) ->
    lists:foldl(fun ({Row, Column}, {FreeCells, Digits}) ->
        CellValue = get_element(Row, Column, Grid),
        if
            CellValue == 0 -> {[{Row, Column}] ++ FreeCells, Digits};
            CellValue /= 0 -> {FreeCells, occupy_digit(Digits, CellValue)}
        end
    end, {[], ?ALL_NUMBERS}, Cells).

-spec append_row_digits(CellRow :: row_type(), Grid :: grid_type(), CellDigits :: non_neg_integer()) -> non_neg_integer().
append_row_digits(CellRow, Grid, CellDigits) -> append_cells_digits(Grid, CellDigits, generate_row(CellRow)).

-spec append_column_digits(CellColumn :: column_type(), Grid :: grid_type(), CellDigits :: non_neg_integer()) -> non_neg_integer().
append_column_digits(CellColumn, Grid, CellDigits) -> append_cells_digits(Grid, CellDigits, generate_column(CellColumn)).

-spec append_square_digits(CellRow :: row_type(),
                           CellColumn :: column_type(),
                           Grid :: grid_type(),
                           CellDigits :: non_neg_integer()) -> non_neg_integer().
append_square_digits(CellRow, CellColumn, Grid, CellDigits) -> append_cells_digits(Grid, CellDigits, generate_square(CellRow, CellColumn)).

-spec append_cells_digits(Grid :: grid_type(), SourceDigits :: non_neg_integer(), Cells :: [coord_type()]) -> non_neg_integer().
append_cells_digits(Grid, SourceDigits, Cells) ->
    lists:foldl(fun ({Row, Column}, Digits) ->
        CellValue = get_element(Row, Column, Grid),
        if
            CellValue == 0 -> Digits;
            CellValue /= 0 -> occupy_digit(Digits, CellValue)
        end
    end, SourceDigits, Cells).

-spec create_digits_info(CellsInfo :: #cell_info{}) -> digits_info().
create_digits_info(CellsInfo) ->
    DigitsInfo = array:new([{size, ?GRID_SIDE}, {fixed, true}, {default, []}]),
    lists:foldl(fun(CellInfo, Dest) -> create_digits_info(CellInfo, Dest) end, DigitsInfo, CellsInfo).

-spec create_digits_info(CellInfo :: #cell_info{}, DigitsInfo :: digits_info()) -> digits_info().
create_digits_info(#cell_info{row = Row, column = Column, constraint = DigitsBinary}, DigitsInfo) ->
    DigitsList = get_free_digits_list(DigitsBinary),
    lists:foldl(fun(Digit, Dest) -> array:set(Digit - 1, [{Row, Column}] ++ array:get(Digit - 1, Dest), Dest) end, DigitsInfo, DigitsList).

-spec get_free_digits_list(DigitsBinary :: non_neg_integer()) -> [digit_type()].
get_free_digits_list(DigitsBinary) ->
    lists:foldl(fun(Digit, DigitsList) ->
        case DigitsBinary band (1 bsl (Digit - 1)) of
            0 -> DigitsList;
            _ -> [Digit] ++ DigitsList
        end
    end, [], lists:seq(1, ?GRID_SIDE)).

-spec choose_cell(CellsInfo :: [#cell_info{}], DigitsInfo :: digits_info()) ->
    {'true', Digit :: digit_type(), Cell :: coord_type()} | 'false'.
choose_cell(CellsInfo, DigitsInfo) ->
    case choose_cells_info(CellsInfo) of
        {true, Digit, {Row, Column}} -> {true, Digit, {Row, Column}};
        false ->
            case choose_digits_info(DigitsInfo) of
                {true, Digit, {Row, Column}} -> {true, Digit, {Row, Column}};
                false -> false
            end
    end.

-spec choose_cells_info(CellsInfo :: [#cell_info{}]) -> {'true', Digit :: digit_type(), Cell :: coord_type()} | 'false'.
choose_cells_info([]) -> false;
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_1} | _Rest]) -> {true, 1, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_2} | _Rest]) -> {true, 2, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_3} | _Rest]) -> {true, 3, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_4} | _Rest]) -> {true, 4, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_5} | _Rest]) -> {true, 5, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_6} | _Rest]) -> {true, 6, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_7} | _Rest]) -> {true, 7, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_8} | _Rest]) -> {true, 8, {Row, Column}};
choose_cells_info([#cell_info{row = Row, column = Column, constraint = ?DIGIT_9} | _Rest]) -> {true, 9, {Row, Column}};
choose_cells_info([_CellInfo | Rest]) -> choose_cells_info(Rest).

-spec choose_digits_info(DigitsInfo :: digits_info()) -> {'true', Digit :: digit_type(), Cell :: coord_type()} | 'false'.
choose_digits_info(DigitsInfo) -> choose_digits_info(DigitsInfo, 1).

-spec choose_digits_info(DigitsInfo :: digits_info(), Digit :: digit_type()) -> {'true', Digit :: digit_type(), Cell :: coord_type()} | 'false'.
choose_digits_info(_DigitsInfo, Digit) when Digit > ?GRID_SIDE -> false;
choose_digits_info(DigitsInfo, Digit) ->
    case array:get(Digit - 1, DigitsInfo) of
        [{Row, Column}] -> {true, Digit, {Row, Column}};
        _Other -> choose_digits_info(DigitsInfo, Digit + 1)
    end.

-spec strikeout_cell(SourceRow :: row_type(),
                     SourceColumn :: column_type(),
                     Digit :: digit_type(),
                     CellsInfo :: [#cell_info{}],
                     DigitsInfo :: digits_info()) ->
    {CellsInfo :: [#cell_info{}], DigitsInfo :: digits_info()}.
strikeout_cell(SourceRow, SourceColumn, Digit, CellsInfo, DigitsInfo) ->
    NewCellsInfo = lists:filter(fun(#cell_info{row = Row, column = Column}) -> (Row /= SourceRow) or (Column /= SourceColumn) end, CellsInfo),
    NewDigitsInfo = array:map(fun(_Index, Cells) -> lists:delete({SourceRow, SourceColumn}, Cells) end, array:set(Digit - 1, [], DigitsInfo)),
    {NewCellsInfo, NewDigitsInfo}.

-spec process_row(SourceRow :: row_type(), Context :: #calc_context{}) -> #calc_context{}.
process_row(SourceRow, Context) ->
    Grid = Context#calc_context.grid,
    {FreeCells, InitCellDigits} = scan_row(SourceRow, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{row = Row, column = Column, constraint = append_square_digits(Row, Column, Grid, append_column_digits(Column, Grid, InitCellDigits))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

-spec process_column(SourceColumn :: column_type(), Context :: #calc_context{}) -> #calc_context{}.
process_column(SourceColumn, Context) ->
    Grid = Context#calc_context.grid,
    {FreeCells, InitCellDigits} = scan_column(SourceColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{row = Row, column = Column, constraint = append_square_digits(Row, Column, Grid, append_row_digits(Row, Grid, InitCellDigits))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

-spec process_square(CellRow :: row_type(), CellColumn :: column_type(), Context :: #calc_context{}) -> #calc_context{}.
process_square(CellRow, CellColumn, Context) ->
    Grid = Context#calc_context.grid,
    {FreeCells, InitCellDigits} = scan_square(CellRow, CellColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{row = Row, column = Column, constraint = append_column_digits(Column, Grid, append_row_digits(Row, Grid, InitCellDigits))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

-spec process_cells(CellsInfo :: [#cell_info{}], Context :: #calc_context{}) -> #calc_context{}.
process_cells(CellsInfo, Context) ->
    DigitsInfo = create_digits_info(CellsInfo),
    process_digits(CellsInfo, DigitsInfo, Context).

-spec process_digits(CellsInfo :: [#cell_info{}], DigitsInfo :: digits_info(), Context :: #calc_context{}) -> #calc_context{}.
process_digits(CellsInfo, DigitsInfo, Context) ->
    case choose_cell(CellsInfo, DigitsInfo) of
        {true, Digit, {Row, Column}} ->
            EmptyCount = Context#calc_context.empty_count,
            UpdatedGrid = set_element(Row, Column, Digit, Context#calc_context.grid),
            UpdatedContext = #calc_context{empty_count = EmptyCount - 1, grid = UpdatedGrid},
            {UpdatedCellsInfo, UpdatedDigitsInfo} = strikeout_cell(Row, Column, Digit, CellsInfo, DigitsInfo),
            process_digits(UpdatedCellsInfo, UpdatedDigitsInfo, UpdatedContext);
        false -> Context
    end.

-spec process_calculation(Context :: #calc_context{}) ->
    {Completed :: boolean(), UpdatedContext :: #calc_context{}} | 'stop_calc'.
process_calculation(Context) when Context#calc_context.empty_count == 0 -> {true, Context};
process_calculation(ContextBefore) ->
    ContextAfterRows = lists:foldl(fun(Row, Context) -> process_row(Row, Context) end, ContextBefore, lists:seq(1, ?GRID_SIDE)),
    ContextAfterColumns = lists:foldl(fun(Column, Context) -> process_column(Column, Context) end, ContextAfterRows, lists:seq(1, ?GRID_SIDE)),
    ContextAfter = lists:foldl(fun({CellRow, CellColumn}, Context) -> process_square(CellRow, CellColumn, Context) end, ContextAfterColumns, ?SQUARES),
    CheckResult = check_calculation(ContextAfter#calc_context.grid),
    if
        CheckResult == false -> stop_calc;
        ContextAfter#calc_context.empty_count < ContextBefore#calc_context.empty_count -> process_calculation(ContextAfter);
        ContextAfter#calc_context.empty_count == ContextBefore#calc_context.empty_count -> {false, ContextAfter}
    end.

-spec check_calculation(Grid :: grid_type()) -> boolean().
check_calculation(Grid) ->
    RowsResult = lists:foldl(fun(Row, Result) -> check_calculation_row(Row, Grid) and Result end, true, lists:seq(1, ?GRID_SIDE)),
    ColumnsResult = lists:foldl(fun(Column, Result) -> check_calculation_column(Column, Grid) and Result end, true, lists:seq(1, ?GRID_SIDE)),
    SquaresResult = lists:foldl(fun({Row, Column}, Result) -> check_calculation_square(Row, Column, Grid) and Result end, true, ?SQUARES),
    RowsResult and ColumnsResult and SquaresResult.

-spec check_calculation_row(SourceRow :: row_type(), Grid :: grid_type()) -> boolean().
check_calculation_row(SourceRow, Grid) ->
    {Cells, CellDigits} = scan_row(SourceRow, Grid),
    ResultDigits = lists:foldl(fun({Row, Column}, Result) ->
        Result bor append_square_digits(Row, Column, Grid, append_column_digits(Column, Grid, CellDigits))
    end, 0, Cells),
    ResultDigits == CellDigits.

-spec check_calculation_column(SourceColumn :: column_type(), Grid :: grid_type()) -> boolean().
check_calculation_column(SourceColumn, Grid) ->
    {Cells, CellDigits} = scan_column(SourceColumn, Grid),
    ResultDigits = lists:foldl(fun({Row, Column}, Result) ->
        Result bor append_square_digits(Row, Column, Grid, append_row_digits(Row, Grid, CellDigits))
    end, 0, Cells),
    ResultDigits == CellDigits.

-spec check_calculation_square(SourceRow :: row_type(), SourceColumn :: column_type(), Grid :: grid_type()) -> boolean().
check_calculation_square(SourceRow, SourceColumn, Grid) ->
    {Cells, CellDigits} = scan_square(SourceRow, SourceColumn, Grid),
    ResultDigits = lists:foldl(fun({Row, Column}, Result) ->
        Result bor append_column_digits(Column, Grid, append_row_digits(Row, Grid, CellDigits))
    end, 0, Cells),
    ResultDigits == CellDigits.

-spec create_context(Grid :: grid_type()) -> #calc_context{}.
create_context(Grid) ->
    EmptyCount = array:foldl(fun(_Index, Value, Result) ->
        if
            Value == 0 -> Result + 1;
            Value > 0 -> Result
        end
    end, 0, Grid),
    #calc_context{empty_count = EmptyCount, grid = Grid}.

-spec find_predict_object(Grid :: grid_type()) -> found_type().
find_predict_object(Grid) ->
    FoundRow = lists:foldl(fun(Row, Result) -> merge_predict_object(Result, scan_row(Row, Grid)) end, {[], 0}, lists:seq(1, ?GRID_SIDE)),
    FoundColumn = lists:foldl(fun(Column, Result) -> merge_predict_object(Result, scan_column(Column, Grid)) end, FoundRow, lists:seq(1, ?GRID_SIDE)),
    lists:foldl(fun({Row, Column}, Result) -> merge_predict_object(Result, scan_square(Row, Column, Grid)) end, FoundColumn, ?SQUARES).

-spec merge_predict_object(OldFoundObject :: found_type(), NewFoundObject :: found_type()) -> found_type().
merge_predict_object({[], 0}, {[], 0}) -> {[], 0};
merge_predict_object({[], 0}, NewFoundObject) -> NewFoundObject;
merge_predict_object(OldFoundObject, {[], 0}) -> OldFoundObject;
merge_predict_object({OldCells, OldDigits}, {NewCells, _NewDigits}) when length(OldCells) =< length(NewCells) -> {OldCells, OldDigits};
merge_predict_object(_OldFoundObject, NewFoundObject) -> NewFoundObject.

-spec create_predict_context(Grid :: grid_type()) -> #predict_context{}.
create_predict_context(Grid) ->
    {Cells, DigitsBinary} = find_predict_object(Grid),
    create_predict_context(Cells, DigitsBinary, Grid).

-spec create_predict_context(Cells :: [coord_type()], DigitsBinary :: non_neg_integer(), Grid :: grid_type()) -> #predict_context{}.
create_predict_context(Cells, DigitsBinary, Grid) ->
    Digits = get_free_digits_list(DigitsBinary),
    CellsInfo = lists:map(fun({Row, Column}) ->
        Constraint = append_square_digits(Row, Column, Grid, append_column_digits(Column, Grid, append_row_digits(Row, Grid, DigitsBinary))),
        #cell_info{row = Row, column = Column, constraint = Constraint}
    end, Cells),
    %% TODO (std_string) : move this into permutations module
    SupLexNumber = numbers:factorial(length(Digits)),
    #predict_context{cells = CellsInfo, digits = Digits, lex_number = -1, sup_lex_number = SupLexNumber - 1, grid = Grid}.

-spec select_next_combination(Context :: #predict_context{}) -> {Digits :: [digit_type()], UpdatedContext :: #predict_context{}} | 'finish'.
select_next_combination(#predict_context{lex_number = Number, sup_lex_number = Number}) -> finish;
select_next_combination(Context) ->
    NextLexNumber = Context#predict_context.lex_number + 1,
    NextDigitCombination = permutations:get_permutation(NextLexNumber, array:fix(array:from_list(Context#predict_context.digits))),
    UpdatedContext = Context#predict_context{lex_number = NextLexNumber},
    case check_digit_combination(Context#predict_context.cells, NextDigitCombination) of
        true -> {NextDigitCombination, UpdatedContext};
        false -> select_next_combination(UpdatedContext)
    end.

-spec check_digit_combination(CellsInfo :: [#cell_info{}], Digits :: [digit_type()]) -> boolean().
check_digit_combination([], []) -> true;
check_digit_combination([], _Digits) -> false;
check_digit_combination([#cell_info{constraint = Constraint} | CellsInfoRest], [Digit | DigitsRest]) ->
    CheckDigit = Constraint band (1 bsl (Digit - 1)),
    if
        CheckDigit == 0 -> false;
        CheckDigit /= 0 -> check_digit_combination(CellsInfoRest, DigitsRest)
    end.

-spec create_prediction(PredictionStack :: [#predict_context{}], CalcContext :: #calc_context{} | 'stop_calc') ->
    {Digits :: [digit_type()], NewPredict :: #predict_context{}, NewPredictionStack :: [#predict_context{}]} | no_return().
create_prediction([], stop_calc) -> error(invalid_operation);
create_prediction([PredictContext | PredictionStackRest], stop_calc) ->
    case select_next_combination(PredictContext) of
        finish -> create_prediction(PredictionStackRest, stop_calc);
        {DigitCombination, UpdatedPredictContext} -> {DigitCombination, UpdatedPredictContext, [UpdatedPredictContext] ++ PredictionStackRest}
    end;
create_prediction([], CalcContext) ->
    {DigitCombination, PredictContext} = select_next_combination(create_predict_context(CalcContext#calc_context.grid)),
    {DigitCombination, PredictContext, [PredictContext]};
create_prediction(PredictionStack, CalcContext) ->
    case select_next_combination(create_predict_context(CalcContext#calc_context.grid)) of
        finish -> create_prediction(PredictionStack, stop_calc);
        {DigitCombination, NewPredictContext} -> {DigitCombination, NewPredictContext, [NewPredictContext] ++ PredictionStack}
    end.

-spec apply_prediction(Digits :: [digit_type()], PredictContext :: #predict_context{}, Grid :: grid_type()) -> grid_type().
apply_prediction(DigitCombination, PredictContext, Grid) -> apply_prediction_impl(DigitCombination, PredictContext#predict_context.cells, Grid).

-spec apply_prediction_impl(Digits :: [digit_type()], CellsInfo :: [#cell_info{}], Grid :: grid_type()) -> grid_type().
apply_prediction_impl([], [], Grid) -> Grid;
apply_prediction_impl([Digit | DigitsRest], [#cell_info{row = Row, column = Column} | CellsInfoRest], Grid) ->
    apply_prediction_impl(DigitsRest, CellsInfoRest, set_element(Row, Column, Digit, Grid)).