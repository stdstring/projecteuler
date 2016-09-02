%% @author std-string

-module(numerical_task_executor).

-include("numerical_task_executor.hrl").

-export([process/4]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(ModuleName :: atom(), WarnTime :: pos_integer(), MaxTime :: pos_integer(), ModuleSourceDir :: string()) -> [result_type()].
process(ModuleName, WarnTime, MaxTime, ModuleSourceDir) ->
    ExpectedResults = ModuleName:get_check_data(),
    ProcessFun = fun({Input, Expected}) -> process(ModuleName, WarnTime, MaxTime, ModuleSourceDir, Input, Expected) end,
    Results = lists:map(ProcessFun, ExpectedResults),
    Results.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(ModuleName :: atom(),
              WarnTime :: pos_integer(),
              MaxTime :: pos_integer(),
              ModuleSourceDir :: string(),
              Input :: term(),
              Expected :: term()) -> result_type().
process(ModuleName, WarnTime, MaxTime, ModuleSourceDir, Input, Expected) ->
    PreparedData = ModuleName:prepare_data(ModuleSourceDir, Input),
    try timer:tc(ModuleName, solve, [PreparedData]) of
        {TimerValue, Actual} ->
            Time = TimerValue div 1000,
            io:format("Execute task = ~p with input = ~p for ~p ms~n", [ModuleName, Input, Time]),
            process_result(ModuleName, WarnTime, MaxTime, Input, Expected, Actual, Time)
    catch
        Exception ->
            io:format("Execute task = ~p with input = ~p failed due to exception~n", [ModuleName, Input]),
            #fail_exec_result{task_name = ModuleName, input = Input, reason = Exception}
    end.

-spec process_result(ModuleName :: atom(),
                     WarnTime :: pos_integer(),
                     MaxTime :: pos_integer(),
                     Input :: term(),
                     Expected :: term(),
                     Actual :: term(),
                     Time :: pos_integer()) -> result_type().
process_result(ModuleName, WarnTime, MaxTime, Input, Value, Value, Time) when Time =< WarnTime ->
    #success_result{task_name = ModuleName, input = Input, status = success, time = Time, warn_time = WarnTime, max_time = MaxTime};
process_result(ModuleName, WarnTime, MaxTime, Input, Value, Value, Time) when (WarnTime < Time) and (Time =< MaxTime) ->
    #success_result{task_name = ModuleName, input = Input, status = warning, time = Time, warn_time = WarnTime, max_time = MaxTime};
process_result(ModuleName, _WarnTime, MaxTime, Input, Value, Value, Time) when Time > MaxTime ->
    #fail_time_result{task_name = ModuleName, input = Input, time = Time, max_time = MaxTime};
process_result(ModuleName, _WarnTime, _MaxTime, Input, Expected, Actual, _Time) ->
    #fail_value_result{task_name = ModuleName, input = Input, expected_value = Expected, actual_value = Actual}.