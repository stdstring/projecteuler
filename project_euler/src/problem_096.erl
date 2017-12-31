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
-define(NUMBER_1 ,2#000000001).
-define(NUMBER_2 ,2#000000010).
-define(NUMBER_3 ,2#000000100).
-define(NUMBER_4 ,2#000001000).
-define(NUMBER_5 ,2#000010000).
-define(NUMBER_6 ,2#000100000).
-define(NUMBER_7 ,2#001000000).
-define(NUMBER_8 ,2#010000000).
-define(NUMBER_9 ,2#100000000).
-define(SQUARES, [{1, 1}, {1, 4}, {1, 7}, {4, 1}, {4, 4}, {4, 7}, {7, 1}, {7, 4}, {7, 7}]).

-record(numbers_binary, {value :: non_neg_integer()}).

%% TODO (std_string) : move into common and use such approach in all other cases
%% TODO (std_string) : think about using records instead of tuples
-type grid() :: array:array(numbers:digit()).
-type row() :: 1..?GRID_SIDE.
-type column() :: 1..?GRID_SIDE.
-type coord() :: {Row :: row(), Column :: column()}.
-type coords() :: [coord()].
-type scan_result() :: {Cells :: coords(), NumbersBinary :: #numbers_binary{}}.
-type choose_cell_result() :: {'true', Number :: numbers:digit(), Cell :: coord()} | 'false'.

-record(grid_case, {name :: string(), grid :: grid()}).
-record(cell_info, {cell :: coord(), constraint :: #numbers_binary{}}).
-record(calc_context, {empty_count :: non_neg_integer(), grid :: grid()}).
-record(predict_context, {cells :: [#cell_info{}],
                          numbers :: array:array(numbers:digit()),
                          lex_number :: non_neg_integer(),
                          sup_lex_number :: non_neg_integer(),
                          grid :: grid()}).

-type cells_info() :: [#cell_info{}].
-type numbers_info() :: array:array(coords() | 'undef').
-type calc_result() :: {'true', Context :: #calc_context{}} | 'false'.
-type prediction_stack() :: [#predict_context{}].
-type prediction() :: {Numbers :: [numbers:digit()], Predict :: #predict_context{}, PredictionStack :: prediction_stack()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_096.dat", 24702}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    Strings = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    convert_data(Strings, []).

-spec solve(PreparedInput :: term()) -> term().
solve(Data) ->
    lists:foldl(fun(#grid_case{name = Name, grid = Grid}, Sum) -> Sum + solve_case(Name, Grid) end, 0, Data).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO (std_string) : move into common and use such approach in all other cases
-spec get_element(Row :: row(), Column :: column(), Grid :: grid()) -> numbers:digit().
get_element(Row, Column, Grid) ->
    Index = (Row - 1) * ?GRID_SIDE + (Column - 1),
    array:get(Index, Grid).

%% TODO (std_string) : move into common and use such approach in all other cases
-spec set_element(Row :: row(), Column :: column(), Value :: numbers:digit(), Grid :: grid()) -> grid().
set_element(Row, Column, Value, Grid) ->
    Index = (Row - 1) * ?GRID_SIDE + (Column - 1),
    array:set(Index, Value, Grid).

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

-spec solve_case(Name :: string(), InitGrid :: grid())-> pos_integer().
solve_case(_Name, InitGrid)->
    Grid = process_grid([], create_calc_context(InitGrid)),
    get_element(1, 1, Grid) * 100 + get_element(1, 2, Grid) * 10 + get_element(1, 3, Grid).

-spec process_grid(PredictionStack :: prediction_stack(), CalcContextBefore :: #calc_context{}) -> grid().
process_grid(PredictionStack, CalcContextBefore) ->
    case process_calculation(CalcContextBefore) of
        stop ->
            {NumbersCombination, PredictContext, UpdatedPredictionStack} = create_prediction(PredictionStack, stop),
            UpdatedGrid = apply_prediction(NumbersCombination, PredictContext, PredictContext#predict_context.grid),
            process_grid(UpdatedPredictionStack, create_calc_context(UpdatedGrid));
        {false, CalcContextAfter} ->
            {NumbersCombination, PredictContext, UpdatedPredictionStack} = create_prediction(PredictionStack, CalcContextAfter),
            UpdatedGrid = apply_prediction(NumbersCombination, PredictContext, CalcContextAfter#calc_context.grid),
            process_grid(UpdatedPredictionStack, create_calc_context(UpdatedGrid));
        {true, CalcContextAfter} -> CalcContextAfter#calc_context.grid
    end.

-spec contains_number(NumbersBinary :: #numbers_binary{}, Number :: numbers:digit()) -> boolean().
contains_number(NumbersBinary, Number) -> (NumbersBinary#numbers_binary.value band (1 bsl (Number - 1))) /= 0.

-spec use_number(NumbersBinary :: #numbers_binary{}, Number :: numbers:digit()) -> #numbers_binary{}.
use_number(NumbersBinary, Number) -> #numbers_binary{value = NumbersBinary#numbers_binary.value band bnot(1 bsl (Number - 1))}.

-spec generate_row(Row :: row()) -> coords().
generate_row(Row) -> lists:map(fun(Column) -> {Row, Column} end, lists:seq(1, ?GRID_SIDE)).

-spec generate_column(Column :: column()) -> coords().
generate_column(Column) -> lists:map(fun(Row) -> {Row, Column} end, lists:seq(1, ?GRID_SIDE)).

-spec generate_square(Row :: row(), Column :: column()) -> coords().
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

-spec scan_row(Row :: row(), Grid :: grid()) -> scan_result().
scan_row(Row, Grid) -> scan_cells(Grid, generate_row(Row)).

-spec scan_column(Column :: column(), Grid :: grid()) -> scan_result().
scan_column(Column, Grid) -> scan_cells(Grid, generate_column(Column)).

-spec scan_square(CellRow :: row(), CellColumn :: column(), Grid :: grid()) -> scan_result().
scan_square(CellRow, CellColumn, Grid) -> scan_cells(Grid, generate_square(CellRow, CellColumn)).

-spec scan_cells(Grid :: grid(), Cells :: coords()) -> scan_result().
scan_cells(Grid, Cells) ->
    lists:foldl(fun ({Row, Column}, {FreeCells, NumbersBinary}) ->
        CellValue = get_element(Row, Column, Grid),
        if
            CellValue == 0 -> {[{Row, Column}] ++ FreeCells, NumbersBinary};
            CellValue /= 0 -> {FreeCells, use_number(NumbersBinary, CellValue)}
        end
    end, {[], #numbers_binary{value = ?ALL_NUMBERS}}, Cells).

-spec append_row(Row :: row(), Grid :: grid(), NumbersBinary :: #numbers_binary{}) -> #numbers_binary{}.
append_row(Row, Grid, NumbersBinary) -> append_cells(generate_row(Row), Grid, NumbersBinary).

-spec append_column(Column :: column(), Grid :: grid(), NumbersBinary :: #numbers_binary{}) -> #numbers_binary{}.
append_column(Column, Grid, NumbersBinary) -> append_cells(generate_column(Column), Grid, NumbersBinary).

-spec append_square(Row :: row(), Column :: column(), Grid :: grid(), NumbersBinary :: #numbers_binary{}) -> #numbers_binary{}.
append_square(Row, Column, Grid, NumbersBinary) -> append_cells(generate_square(Row, Column), Grid, NumbersBinary).

-spec append_cells(Cells :: coords(), Grid :: grid(), SourceNumbersBinary :: #numbers_binary{}) -> #numbers_binary{}.
append_cells(Cells, Grid, SourceNumbersBinary) ->
    lists:foldl(fun ({Row, Column}, NumbersBinary) ->
        CellValue = get_element(Row, Column, Grid),
        if
            CellValue == 0 -> NumbersBinary;
            CellValue /= 0 -> use_number(NumbersBinary, CellValue)
        end
    end, SourceNumbersBinary, Cells).

-spec create_numbers_info(CellsInfo :: cells_info()) -> numbers_info().
create_numbers_info(CellsInfo) ->
    NumbersInfo = array:new([{size, ?GRID_SIDE}, {fixed, true}, {default, undef}]),
    lists:foldl(fun(CellInfo, Dest) -> append_cell_info(CellInfo, Dest) end, NumbersInfo, CellsInfo).

-spec append_cell_info(CellInfo :: #cell_info{}, NumbersInfo :: numbers_info()) -> numbers_info().
append_cell_info(#cell_info{cell = Cell, constraint = NumbersBinary}, NumbersInfo) ->
    NumbersList = lists:filter(fun(Number) -> contains_number(NumbersBinary, Number) end, lists:seq(1, ?GRID_SIDE)),
    lists:foldl(fun(Number, Dest) ->
        Cells = case array:get(Number - 1, Dest) of
                    undef -> [];
                    Other -> Other
                end,
        array:set(Number - 1, [Cell] ++ Cells, Dest)
    end, NumbersInfo, NumbersList).

-spec choose_cell(CellsInfo :: cells_info(), NumbersInfo :: numbers_info()) -> choose_cell_result().
choose_cell(CellsInfo, NumbersInfo) ->
    case choose_cells_info(CellsInfo) of
        {true, Number, Cell} -> {true, Number, Cell};
        false ->
            case choose_numbers_info(NumbersInfo) of
                {true, Number, Cell} -> {true, Number, Cell};
                false -> false
            end
    end.

-spec choose_cells_info(CellsInfo :: cells_info()) -> choose_cell_result().
choose_cells_info([]) -> false;
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_1}} | _Rest]) -> {true, 1, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_2}} | _Rest]) -> {true, 2, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_3}} | _Rest]) -> {true, 3, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_4}} | _Rest]) -> {true, 4, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_5}} | _Rest]) -> {true, 5, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_6}} | _Rest]) -> {true, 6, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_7}} | _Rest]) -> {true, 7, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_8}} | _Rest]) -> {true, 8, Cell};
choose_cells_info([#cell_info{cell = Cell, constraint = #numbers_binary{value = ?NUMBER_9}} | _Rest]) -> {true, 9, Cell};
choose_cells_info([_CellInfo | Rest]) -> choose_cells_info(Rest).

-spec choose_numbers_info(NumbersInfo :: numbers_info()) -> choose_cell_result().
choose_numbers_info(NumbersInfo) -> choose_numbers_info(NumbersInfo, lists:seq(1, ?GRID_SIDE)).

-spec choose_numbers_info(NumbersInfo :: numbers_info(), Numbers :: [numbers:digit()]) -> choose_cell_result().
choose_numbers_info(_NumbersInfo, []) -> false;
choose_numbers_info(NumbersInfo, [Number | NumbersRest]) ->
    case array:get(Number - 1, NumbersInfo) of
        [Cell] -> {true, Number, Cell};
        _Other -> choose_numbers_info(NumbersInfo, NumbersRest)
    end.

-spec check_calculation(CellsInfo :: cells_info(), NumbersInfo :: numbers_info()) -> boolean().
check_calculation(CellsInfo, NumbersInfo) ->
    lists:all(fun(#cell_info{constraint = #numbers_binary{value = Value}}) -> Value /= 0 end, CellsInfo) and
    array:foldl(fun(_Index, Cells, Result) -> Result and (Cells /= []) end, true, NumbersInfo).

-spec process_row(SourceRow :: row(), CalcResult :: calc_result()) -> calc_result().
process_row(_SourceRow, false) -> false;
process_row(SourceRow, {true, Context}) ->
    Grid = Context#calc_context.grid,
    {FreeCells, NumbersBinary} = scan_row(SourceRow, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{cell = {Row, Column}, constraint = append_square(Row, Column, Grid, append_column(Column, Grid, NumbersBinary))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

-spec process_column(SourceColumn :: column(), CalcResult :: calc_result()) -> calc_result().
process_column(_SourceColumn, false) -> false;
process_column(SourceColumn, {true, Context}) ->
    Grid = Context#calc_context.grid,
    {FreeCells, NumbersBinary} = scan_column(SourceColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{cell = {Row, Column}, constraint = append_square(Row, Column, Grid, append_row(Row, Grid, NumbersBinary))}
    end, FreeCells),
    process_cells(CellsInfo, Context).

-spec process_square(CellRow :: row(), CellColumn :: column(), CalcResult :: calc_result()) -> calc_result().
process_square(_CellRow, _CellColumn, false) -> false;
process_square(CellRow, CellColumn, {true, Context}) ->
    Grid = Context#calc_context.grid,
    {FreeCells, NumbersBinary} = scan_square(CellRow, CellColumn, Grid),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{cell = {Row, Column}, constraint = append_column(Column, Grid, append_row(Row, Grid, NumbersBinary))}
     end, FreeCells),
     process_cells(CellsInfo, Context).

-spec process_cells(CellsInfo :: cells_info(), Context :: #calc_context{}) -> calc_result().
process_cells(CellsInfo, Context) ->
    NumbersInfo = create_numbers_info(CellsInfo),
    process_cells(CellsInfo, NumbersInfo, Context).

-spec process_cells(CellsInfo :: cells_info(), NumbersInfo :: numbers_info(), Context :: #calc_context{}) -> calc_result().
process_cells(CellsInfo, NumbersInfo, Context) ->
    case choose_cell(CellsInfo, NumbersInfo) of
        {true, Number, {Row, Column}} ->
            EmptyCount = Context#calc_context.empty_count,
            UpdatedGrid = set_element(Row, Column, Number, Context#calc_context.grid),
            UpdatedContext = #calc_context{empty_count = EmptyCount - 1, grid = UpdatedGrid},
            {UpdatedCellsInfo, UpdatedNumbersInfo} = update_info({Row, Column}, Number, CellsInfo, NumbersInfo),
            case check_calculation(UpdatedCellsInfo, UpdatedNumbersInfo) of
                true -> process_cells(UpdatedCellsInfo, UpdatedNumbersInfo, UpdatedContext);
                false -> false
            end;
        false -> {true, Context}
    end.

-spec update_info(Cell :: coord(), Number :: numbers:digit(), CellsInfo :: cells_info(), NumbersInfo :: numbers_info()) ->
    {CellsInfo :: cells_info(), NumbersInfo :: numbers_info()}.
update_info(Cell, Number, CellsInfo, NumbersInfo) ->
    UpdatedCellsInfo = lists:filter(fun(CellInfo) -> CellInfo#cell_info.cell /= Cell end, CellsInfo),
    UpdatedNumbersInfo = array:map(fun(_Index, Cells) ->
        if
            Cells == undef -> undef;
            Cells /= undef -> lists:delete(Cell, Cells)
        end
    end, array:set(Number - 1, undef, NumbersInfo)),
    {UpdatedCellsInfo, UpdatedNumbersInfo}.

-spec process_calculation(Context :: #calc_context{}) -> {Completed :: boolean(), UpdatedContext :: #calc_context{}} | 'stop'.
process_calculation(Context) when Context#calc_context.empty_count == 0 -> {true, Context};
process_calculation(ContextBefore) ->
    ResultAfterRows = lists:foldl(fun(Row, Result) -> process_row(Row, Result) end, {true, ContextBefore}, lists:seq(1, ?GRID_SIDE)),
    ResultAfterColumns = lists:foldl(fun(Column, Result) -> process_column(Column, Result) end, ResultAfterRows, lists:seq(1, ?GRID_SIDE)),
    ResultAfter = lists:foldl(fun({CellRow, CellColumn}, Result) -> process_square(CellRow, CellColumn, Result) end, ResultAfterColumns, ?SQUARES),
    case ResultAfter of
        false ->  stop;
        {true, ContextAfter} when ContextAfter#calc_context.empty_count < ContextBefore#calc_context.empty_count -> process_calculation(ContextAfter);
        {true, ContextAfter} when ContextAfter#calc_context.empty_count == ContextBefore#calc_context.empty_count -> {false, ContextAfter}
    end.

-spec create_calc_context(Grid :: grid()) -> #calc_context{}.
create_calc_context(Grid) ->
    EmptyCount = array:foldl(fun(_Index, Value, Result) -> Result + (if Value == 0 -> 1; Value > 0 -> 0 end) end, 0, Grid),
    #calc_context{empty_count = EmptyCount, grid = Grid}.

-spec find_predict_object(Grid :: grid()) -> scan_result().
find_predict_object(Grid) ->
    InitObj = {[], #numbers_binary{value = 0}},
    RowObj = lists:foldl(fun(Row, Result) -> merge_predict_object(Result, scan_row(Row, Grid)) end, InitObj, lists:seq(1, ?GRID_SIDE)),
    ColumnObj = lists:foldl(fun(Column, Result) -> merge_predict_object(Result, scan_column(Column, Grid)) end, RowObj, lists:seq(1, ?GRID_SIDE)),
    lists:foldl(fun({Row, Column}, Result) -> merge_predict_object(Result, scan_square(Row, Column, Grid)) end, ColumnObj, ?SQUARES).

-spec merge_predict_object(LeftObj :: scan_result(), RightObj :: scan_result()) -> scan_result().
merge_predict_object({[], #numbers_binary{value = 0}}, {[], #numbers_binary{value = 0}}) -> {[], #numbers_binary{value = 0}};
merge_predict_object({[], #numbers_binary{value = 0}}, RightObj) -> RightObj;
merge_predict_object(LeftObj, {[], #numbers_binary{value = 0}}) -> LeftObj;
merge_predict_object({LeftCells, LeftNumbersBinary}, {RightCells, _RightNumbersBinary}) when length(LeftCells) =< length(RightCells) -> {LeftCells, LeftNumbersBinary};
merge_predict_object(_LeftObj, RightObj) -> RightObj.

-spec create_predict_context(Grid :: grid()) -> #predict_context{}.
create_predict_context(Grid) ->
    {Cells, NumbersBinary} = find_predict_object(Grid),
    create_predict_context(Grid, Cells, NumbersBinary).

-spec create_predict_context(Grid :: grid(), Cells :: coords(), NumbersBinary :: #numbers_binary{}) -> #predict_context{}.
create_predict_context(Grid, Cells, NumbersBinary) ->
    Numbers = lists:filter(fun(Number) -> contains_number(NumbersBinary, Number) end, lists:seq(1, ?GRID_SIDE)),
    CellsInfo = lists:map(fun({Row, Column}) ->
        #cell_info{cell = {Row, Column}, constraint = append_square(Row, Column, Grid, append_column(Column, Grid, append_row(Row, Grid, NumbersBinary)))}
    end, Cells),
    SupLexNumber = permutations:get_lexicographical_number_sup(array:from_list(Numbers)),
    #predict_context{cells = CellsInfo, numbers = array:fix(array:from_list(Numbers)), lex_number = -1, sup_lex_number = SupLexNumber - 1, grid = Grid}.

-spec select_next_combination(Context :: #predict_context{}) -> {Numbers :: [numbers:digit()], UpdatedContext :: #predict_context{}} | 'finish'.
select_next_combination(#predict_context{lex_number = Number, sup_lex_number = Number}) -> finish;
select_next_combination(Context) ->
    NextLexNumber = Context#predict_context.lex_number + 1,
    NextNumbersCombination = permutations:get_permutation(NextLexNumber, Context#predict_context.numbers),
    UpdatedContext = Context#predict_context{lex_number = NextLexNumber},
    case check_combination(Context#predict_context.cells, NextNumbersCombination) of
        true -> {NextNumbersCombination, UpdatedContext};
        false -> select_next_combination(UpdatedContext)
    end.

-spec check_combination(CellsInfo :: cells_info(), Numbers :: [numbers:digit()]) -> boolean().
check_combination([], []) -> true;
check_combination([], _Numbers) -> false;
check_combination([#cell_info{constraint = Constraint} | CellsInfoRest], [Number | NumbersRest]) ->
    case contains_number(Constraint, Number) of
        true -> check_combination(CellsInfoRest, NumbersRest);
        false -> false
    end.

-spec create_prediction(PredictionStack :: prediction_stack(), CalcContext :: #calc_context{} | 'stop') -> prediction() | no_return().
create_prediction([], stop) -> error(invalid_operation);
create_prediction([PredictContext | PredictionStackRest], stop) ->
    case select_next_combination(PredictContext) of
        finish -> create_prediction(PredictionStackRest, stop);
        {NumbersCombination, UpdatedPredictContext} -> {NumbersCombination, UpdatedPredictContext, [UpdatedPredictContext] ++ PredictionStackRest}
    end;
create_prediction([], CalcContext) ->
    {NumbersCombination, PredictContext} = select_next_combination(create_predict_context(CalcContext#calc_context.grid)),
    {NumbersCombination, PredictContext, [PredictContext]};
create_prediction(PredictionStack, CalcContext) ->
    case select_next_combination(create_predict_context(CalcContext#calc_context.grid)) of
        finish -> create_prediction(PredictionStack, stop);
        {NumbersCombination, NewPredictContext} -> {NumbersCombination, NewPredictContext, [NewPredictContext] ++ PredictionStack}
    end.

-spec apply_prediction(NumbersCombination :: [numbers:digit()], PredictContext :: #predict_context{}, Grid :: grid()) -> grid().
apply_prediction(NumbersCombination, PredictContext, Grid) -> apply_prediction_impl(NumbersCombination, PredictContext#predict_context.cells, Grid).

-spec apply_prediction_impl(NumbersCombination :: [numbers:digit()], CellsInfo :: cells_info(), Grid :: grid()) -> grid().
apply_prediction_impl([], [], Grid) -> Grid;
apply_prediction_impl([Number | NumbersRest], [#cell_info{cell = {Row, Column}} | CellsInfoRest], Grid) ->
    apply_prediction_impl(NumbersRest, CellsInfoRest, set_element(Row, Column, Number, Grid)).