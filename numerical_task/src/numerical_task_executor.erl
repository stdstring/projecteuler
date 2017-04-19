%% @author std-string

-module(numerical_task_executor).

-include("numerical_task_executor.hrl").

-export([process/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(ModuleName :: atom(), ModuleSourceDir :: string(), TimeThresholds :: #time_thresholds{}) -> [result_type()].
process(ModuleName, ModuleSourceDir, TimeThresholds) ->
    ExpectedResults = ModuleName:get_check_data(),
    ProcessFun = fun({Input, Expected}) -> process(ModuleName, ModuleSourceDir, TimeThresholds, Input, Expected) end,
    lists:map(ProcessFun, ExpectedResults).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(ModuleName :: atom(), ModuleSourceDir :: string(), TimeThresholds :: #time_thresholds{}, Input :: term(), Expected :: term()) -> result_type().
process(ModuleName, ModuleSourceDir, TimeThresholds, Input, Expected) ->
    PreparedData = ModuleName:prepare_data(ModuleSourceDir, Input),
    try timer:tc(ModuleName, solve, [PreparedData]) of
        {TimerValue, Actual} ->
            Time = TimerValue div 1000,
            io:format("Execute task = ~p with input = ~p for ~p ms~n", [ModuleName, Input, Time]),
            process_result(ModuleName, TimeThresholds, Input, Expected, Actual, Time)
    catch
        Exception ->
            io:format("Execute task = ~p with input = ~p failed due to exception~n", [ModuleName, Input]),
            #fail_exec_result{task_name = ModuleName, input = Input, reason = Exception}
    end.

-spec process_result(ModuleName :: atom(),
                     TimeThresholds :: #time_thresholds{},
                     Input :: term(),
                     Expected :: term(),
                     Actual :: term(),
                     Time :: pos_integer()) -> result_type().
process_result(ModuleName, TimeThresholds, Input, Value, Value, Time) when Time =< TimeThresholds#time_thresholds.attention ->
    #success_result{task_name = ModuleName, input = Input, status = success, time = Time, thresholds = TimeThresholds};
process_result(ModuleName, TimeThresholds, Input, Value, Value, Time) when Time =< TimeThresholds#time_thresholds.warning ->
    #success_result{task_name = ModuleName, input = Input, status = attention, time = Time, thresholds = TimeThresholds};
process_result(ModuleName, TimeThresholds, Input, Value, Value, Time) when Time =< TimeThresholds#time_thresholds.max ->
    #success_result{task_name = ModuleName, input = Input, status = warning, time = Time, thresholds = TimeThresholds};
process_result(ModuleName, TimeThresholds, Input, Value, Value, Time) when Time > TimeThresholds#time_thresholds.max ->
    #fail_time_result{task_name = ModuleName, input = Input, time = Time, max_time = TimeThresholds#time_thresholds.max};
process_result(ModuleName, _TimeThresholds, Input, Expected, Actual, _Time) ->
    #fail_value_result{task_name = ModuleName, input = Input, expected_value = Expected, actual_value = Actual}.