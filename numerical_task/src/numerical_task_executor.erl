-module(numerical_task_executor).

-include("numerical_task_executor.hrl").

-export([process/3]).

-spec process(ModuleName :: atom(), MaxTime :: pos_integer(), ModuleSourceDir :: string()) ->
    [#success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}].
process(ModuleName, MaxTime, ModuleSourceDir) ->
    ExpectedResults = ModuleName:get_check_data(),
    ProcessFun = fun({Input, Expected}) -> process(ModuleName, MaxTime, ModuleSourceDir, Input, Expected) end,
    Results = lists:map(ProcessFun, ExpectedResults),
    Results.

-spec process(ModuleName :: atom(), MaxTime :: pos_integer(), ModuleSourceDir :: string(), Input :: term(), Expected :: term()) ->
    #success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}.
process(ModuleName, MaxTime, ModuleSourceDir, Input, Expected) ->
    PreparedData = ModuleName:prepare_data(ModuleSourceDir, Input),
    try timer:tc(ModuleName, solve, [PreparedData]) of
        {Time, Actual} ->
            io:format("Execute task = ~p with input = ~p for ~p mcs~n", [ModuleName, Input, Time]),
            process_result(ModuleName, Input, Expected, Actual, Time, MaxTime)
    catch
        Exception ->
            io:format("Execute task = ~p with input = ~p failed due to exception~n", [ModuleName, Input]),
            #fail_exec_result{task_name = ModuleName, input = Input, reason = Exception}
    end.

-spec process_result(ModuleName :: atom(), Input :: term(), Expected :: term(), Actual :: term(), Time :: pos_integer(), MaxTime :: pos_integer()) ->
    #success_result{} | #fail_time_result{} | #fail_value_result{}.
process_result(ModuleName, Input, Value, Value, Time, MaxTime) when Time =< MaxTime ->
    #success_result{task_name = ModuleName, input = Input, current_time = Time, max_time = MaxTime};
process_result(ModuleName, Input, Value, Value, Time, MaxTime) when Time > MaxTime ->
    #fail_time_result{task_name = ModuleName, input = Input, current_time = Time, max_time = MaxTime};
process_result(ModuleName, Input, Expected, Actual, _Time, _MaxTime) ->
    #fail_value_result{task_name = ModuleName, input = Input, expected_value = Expected, actual_value = Actual}.