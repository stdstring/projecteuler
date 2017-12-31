%% @author std-string

-module(grid_path_searcher).
-export([search/6]).

-include("grid_def.hrl").

-type value_builder_fun() :: fun((AccValue :: term(), PointValue :: term()) -> term()).
-type value_comparator_fun() :: fun((LValue :: term(), RValue :: term()) -> compare:compare_result()).
-type next_points_provider_type() :: fun((Point :: point_type(), RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> [point_type()]).
-type result_item_type() :: {Path :: [point_type()], Value :: term()}.
-type result_type() :: array:array(array:array(result_item_type() | 'undef')).

-record(process_data, {grid :: grid_type(),
                       row_max :: pos_integer(),
                       column_max :: pos_integer(),
                       value_builder :: value_builder_fun(),
                       value_comparator :: value_comparator_fun(),
                       next_points_provider :: next_points_provider_type()}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec search(Grid :: grid_type(),
             InitPoints :: [point_type()],
             ResultPoints :: [point_type()],
             ValueBuilder :: value_builder_fun(),
             ValueComparator :: value_comparator_fun(),
             NextPointsProvider :: next_points_provider_type()) ->
    [point_type()].
search(Grid, InitPoints, ResultPoints, ValueBuilder, ValueComparator, NextPointsProvider) ->
    RowMax = grid_helper:get_row_count(Grid),
    ColumnMax = grid_helper:get_column_count(Grid),
    ProcessData = #process_data{grid = Grid,
                                row_max = RowMax,
                                column_max = ColumnMax,
                                value_builder = ValueBuilder,
                                value_comparator = ValueComparator,
                                next_points_provider = NextPointsProvider},
    InitResult = init_result_storage(InitPoints, Grid, create_result_storage(RowMax, ColumnMax)),
    Result = process_grid(InitPoints, ProcessData, InitResult),
    {Value, Path} = process_result(ResultPoints, Result, ValueComparator, undef),
    {Value, lists:reverse(Path)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_result_storage(RowMax :: pos_integer(), ColumnMax :: pos_integer()) -> result_type().
create_result_storage(RowMax, ColumnMax) ->
    create_result_storage(RowMax, 0, ColumnMax, array:new([{size, RowMax}, {fixed, true}])).

-spec create_result_storage(RowMax :: pos_integer(), RowIndex :: non_neg_integer(), ColumnMax :: pos_integer(), Result :: result_type()) ->  result_type().
create_result_storage(RowMax, RowMax, _ColumnMax, Result) -> Result;
create_result_storage(RowMax, RowIndex, ColumnMax, Result) ->
    NewRow = array:new([{size, ColumnMax}, {fixed, true}, {default, undef}]),
    create_result_storage(RowMax, RowIndex + 1, ColumnMax, array:set(RowIndex, NewRow, Result)).

-spec init_result_storage(InitPoints :: [point_type()], Grid :: grid_type(), Result :: result_type()) -> result_type().
init_result_storage(Points, Grid, Result) ->
    InitFun = fun({Row, Column}, Storage) -> grid_helper:set_value(Row, Column, {grid_helper:get_value(Row, Column, Grid), [{Row, Column}]}, Storage) end,
    lists:foldl(InitFun, Result, Points).

-spec process_grid(Points :: [point_type()], ProcessData :: #process_data{}, Result :: result_type()) -> result_type().
process_grid(Points, ProcessData, Result) ->
    case process_points(Points, ProcessData, Result, []) of
        {NextResult, []} -> NextResult;
        {NextResult, NextPoints} -> process_grid(NextPoints, ProcessData, NextResult)
    end.

-spec process_points(Points :: [point_type()], ProcessData :: #process_data{}, Result :: result_type(), Dest :: [point_type()]) ->
    {Result :: result_type(), Points :: [point_type()]}.
process_points([], _ProcessData, Result, Dest) -> {Result, Dest};
process_points([{Row, Column} | PointsRest], ProcessData, Result, Dest) ->
    RowMax = ProcessData#process_data.row_max,
    ColumnMax = ProcessData#process_data.column_max,
    NextPointsProvider = ProcessData#process_data.next_points_provider,
    ActualRecord = grid_helper:get_value(Row, Column, Result),
    NextPoints = NextPointsProvider({Row, Column}, RowMax, ColumnMax),
    {NextResult, AcceptedNextPoints} = process_next_points(ActualRecord, NextPoints, ProcessData, Result, []),
    process_points(PointsRest, ProcessData, NextResult, AcceptedNextPoints ++ Dest).

-spec process_next_points(ActualRecord :: {Value :: term(), Path :: [point_type()]},
                          NextPoints :: [point_type()],
                          ProcessData :: #process_data{},
                          Result :: result_type(),
                          Dest :: [point_type]) -> {Result :: result_type(), Points :: [point_type()]}.
process_next_points(_ActualRecord, [], _ProcessData, Result, Dest) -> {Result, Dest};
process_next_points(ActualRecord, [{RowNext, ColumnNext} | NextPointsRest], ProcessData, Result, Dest) ->
    Grid = ProcessData#process_data.grid,
    PointValue = grid_helper:get_value(RowNext, ColumnNext, Grid),
    NextRecord = grid_helper:get_value(RowNext, ColumnNext, Result),
    {NewResult, NextPointDest} = process_next_point(ActualRecord, NextRecord, {RowNext, ColumnNext}, PointValue, ProcessData, Result),
    process_next_points(ActualRecord, NextPointsRest, ProcessData, NewResult, NextPointDest ++ Dest).

-spec process_next_point(ActualRecord :: {Value :: term(), Path :: [point_type()]},
                         NextRecord :: {Value :: term(), Path :: [point_type()]} | 'undef',
                         Point :: point_type(),
                         PointValue :: term(),
                         ProcessData :: #process_data{},
                         Result :: result_type()) -> {Result :: result_type(), Points :: [point_type()]}.
process_next_point({ActualValue, ActualPath}, undef, {RowNext, ColumnNext}, PointValue, ProcessData, Result) ->
    ValueBuilder = ProcessData#process_data.value_builder,
    CalculatedValue = ValueBuilder(ActualValue, PointValue),
    NewPath = [{RowNext, ColumnNext}] ++ ActualPath,
    {grid_helper:set_value(RowNext, ColumnNext, {CalculatedValue, NewPath}, Result), [{RowNext, ColumnNext}]};
process_next_point({ActualValue, ActualPath}, {NextValue, _NextPath}, {RowNext, ColumnNext}, PointValue, ProcessData, Result) ->
    ValueBuilder = ProcessData#process_data.value_builder,
    CalculatedValue = ValueBuilder(ActualValue, PointValue),
    ValueComparator = ProcessData#process_data.value_comparator,
    case ValueComparator(CalculatedValue, NextValue) of
        right -> {Result, []};
        equal -> {Result, []};
        left ->
            NewPath = [{RowNext, ColumnNext}] ++ ActualPath,
            {grid_helper:set_value(RowNext, ColumnNext, {CalculatedValue, NewPath}, Result), [{RowNext, ColumnNext}]}
    end.

-spec process_result(Points :: [point_type()],
                     Result :: result_type(),
                     ValueComparator :: value_comparator_fun(),
                     ResultValue :: {Value :: term(), Path :: [point_type()]} | 'undef') ->
    {Value :: term(), Path :: [point_type()]}.
process_result([], _Result, _ValueComparator, Value) -> Value;
process_result([{Row, Column} | PointsRest], Result, ValueComparator, undef) ->
    process_result(PointsRest, Result, ValueComparator, grid_helper:get_value(Row, Column, Result));
process_result([{Row, Column} | PointsRest], Result, ValueComparator, {Value, Path}) ->
    {CurrentValue, CurrentPath} = grid_helper:get_value(Row, Column, Result),
    case ValueComparator(CurrentValue, Value) of
        right -> process_result(PointsRest, Result, ValueComparator, {Value, Path});
        equal -> process_result(PointsRest, Result, ValueComparator, {Value, Path});
        left -> process_result(PointsRest, Result, ValueComparator, {CurrentValue, CurrentPath})
    end.
