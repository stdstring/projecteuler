%% @author std-string

-module(grid_path).
-export([search/6]).

-type value_builder_fun() :: fun((AccValue :: term(), PointValue :: term()) -> term()).
-type next_points_provider_type() :: fun((Point :: grid:point_type(), RowMax :: grid:row_type(), ColumnMax :: grid:column_type()) -> [grid:point_type()]).
-type result_type() :: {Value :: term(), Path :: [grid:point_type()]}.
-type grid_result_type() :: grid:grid(result_type() | 'undef').

-record(process_data, {grid :: grid:grid(term()),
                       row_max :: grid:row_type(),
                       column_max :: grid:column_type(),
                       value_builder :: value_builder_fun(),
                       value_comparator :: compare:comparator_fun(),
                       next_points_provider :: next_points_provider_type()}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec search(Grid :: grid:grid(term()),
             InitPoints :: [grid:point_type()],
             ResultPoints :: [grid:point_type()],
             ValueBuilder :: value_builder_fun(),
             ValueComparator :: compare:comparator_fun(),
             NextPointsProvider :: next_points_provider_type()) -> result_type().
search(Grid, InitPoints, ResultPoints, ValueBuilder, ValueComparator, NextPointsProvider) ->
    RowCount = grid:get_row_count(Grid),
    ColumnCount = grid:get_column_count(Grid),
    ProcessData = #process_data{grid = Grid,
                                row_max = RowCount,
                                column_max = ColumnCount,
                                value_builder = ValueBuilder,
                                value_comparator = ValueComparator,
                                next_points_provider = NextPointsProvider},
    InitResult = init_result_storage(InitPoints, Grid, grid:create(RowCount, ColumnCount, undef)),
    Result = process_grid(InitPoints, ProcessData, InitResult),
    {Value, Path} = process_result(ResultPoints, Result, ValueComparator, undef),
    {Value, lists:reverse(Path)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec init_result_storage(InitPoints :: [grid:point_type()], Grid :: grid:grid(term()), Result :: grid_result_type()) -> grid_result_type().
init_result_storage(Points, Grid, Result) ->
    InitFun = fun({Row, Column}, Storage) -> grid:set_value(Row, Column, {grid:get_value(Row, Column, Grid), [{Row, Column}]}, Storage) end,
    lists:foldl(InitFun, Result, Points).

-spec process_grid(Points :: [grid:point_type()], ProcessData :: #process_data{}, Result :: grid_result_type()) -> grid_result_type().
process_grid(Points, ProcessData, Result) ->
    case process_points(Points, ProcessData, Result, []) of
        {NextResult, []} -> NextResult;
        {NextResult, NextPoints} -> process_grid(NextPoints, ProcessData, NextResult)
    end.

-spec process_points(Points :: [grid:point_type()], ProcessData :: #process_data{}, Result :: grid_result_type(), Dest :: [grid:point_type()]) ->
    {Result :: grid_result_type(), Points :: [grid:point_type()]}.
process_points([], _ProcessData, Result, Dest) -> {Result, Dest};
process_points([{Row, Column} | PointsRest], ProcessData, Result, Dest) ->
    RowMax = ProcessData#process_data.row_max,
    ColumnMax = ProcessData#process_data.column_max,
    NextPointsProvider = ProcessData#process_data.next_points_provider,
    ActualRecord = grid:get_value(Row, Column, Result),
    NextPoints = NextPointsProvider({Row, Column}, RowMax, ColumnMax),
    {NextResult, AcceptedNextPoints} = process_next_points(ActualRecord, NextPoints, ProcessData, Result, []),
    process_points(PointsRest, ProcessData, NextResult, AcceptedNextPoints ++ Dest).

-spec process_next_points(ActualRecord :: {Value :: term(), Path :: [grid:point_type()]},
                          NextPoints :: [grid:point_type()],
                          ProcessData :: #process_data{},
                          Result :: grid_result_type(),
                          Dest :: [grid:point_type()]) -> {Result :: grid_result_type(), Points :: [grid:point_type()]}.
process_next_points(_ActualRecord, [], _ProcessData, Result, Dest) -> {Result, Dest};
process_next_points(ActualRecord, [{RowNext, ColumnNext} | NextPointsRest], ProcessData, Result, Dest) ->
    Grid = ProcessData#process_data.grid,
    PointValue = grid:get_value(RowNext, ColumnNext, Grid),
    NextRecord = grid:get_value(RowNext, ColumnNext, Result),
    {NewResult, NextPointDest} = process_next_point(ActualRecord, NextRecord, {RowNext, ColumnNext}, PointValue, ProcessData, Result),
    process_next_points(ActualRecord, NextPointsRest, ProcessData, NewResult, NextPointDest ++ Dest).

-spec process_next_point(ActualRecord :: {Value :: term(), Path :: [grid:point_type()]},
                         NextRecord :: {Value :: term(), Path :: [grid:point_type()]} | 'undef',
                         Point :: grid:point_type(),
                         PointValue :: term(),
                         ProcessData :: #process_data{},
                         Result :: grid_result_type()) -> {Result :: grid_result_type(), Points :: [grid:point_type()]}.
process_next_point({ActualValue, ActualPath}, undef, {RowNext, ColumnNext}, PointValue, ProcessData, Result) ->
    ValueBuilder = ProcessData#process_data.value_builder,
    CalculatedValue = ValueBuilder(ActualValue, PointValue),
    NewPath = [{RowNext, ColumnNext}] ++ ActualPath,
    {grid:set_value(RowNext, ColumnNext, {CalculatedValue, NewPath}, Result), [{RowNext, ColumnNext}]};
process_next_point({ActualValue, ActualPath}, {NextValue, _NextPath}, {RowNext, ColumnNext}, PointValue, ProcessData, Result) ->
    ValueBuilder = ProcessData#process_data.value_builder,
    CalculatedValue = ValueBuilder(ActualValue, PointValue),
    ValueComparator = ProcessData#process_data.value_comparator,
    case ValueComparator(CalculatedValue, NextValue) of
        right -> {Result, []};
        equal -> {Result, []};
        left ->
            NewPath = [{RowNext, ColumnNext}] ++ ActualPath,
            {grid:set_value(RowNext, ColumnNext, {CalculatedValue, NewPath}, Result), [{RowNext, ColumnNext}]}
    end.

-spec process_result(Points :: [grid:point_type()],
                     Result :: grid_result_type(),
                     ValueComparator :: compare:comparator_fun(),
                     ResultValue :: result_type() | 'undef') -> result_type().
process_result([], _Result, _ValueComparator, Value) -> Value;
process_result([{Row, Column} | PointsRest], Result, ValueComparator, undef) ->
    process_result(PointsRest, Result, ValueComparator, grid:get_value(Row, Column, Result));
process_result([{Row, Column} | PointsRest], Result, ValueComparator, {Value, Path}) ->
    {CurrentValue, CurrentPath} = grid:get_value(Row, Column, Result),
    case ValueComparator(CurrentValue, Value) of
        right -> process_result(PointsRest, Result, ValueComparator, {Value, Path});
        equal -> process_result(PointsRest, Result, ValueComparator, {Value, Path});
        left -> process_result(PointsRest, Result, ValueComparator, {CurrentValue, CurrentPath})
    end.
