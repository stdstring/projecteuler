%% @author std-string

-module(numerical_task_list_executor).

-include("numerical_task_executor.hrl").

-export([process/1]).

-type module_def() :: {ModuleName :: atom(), AttentionTime :: pos_integer(), WarningTime :: pos_integer(), MaxTime :: pos_integer()} | atom().

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(ConfigFilename :: string()) -> 'ok'.
process(ConfigFilename) ->
    {ModulePath, AdditionalPathList, ModuleDefList} = load_utils:read_erlang_term(filename:absname(ConfigFilename)),
    code:add_patha(filename:absname(ModulePath)),
    lists:foreach(fun(Path) -> code:add_patha(filename:absname(Path)) end, AdditionalPathList),
    Results = process_tasks(ModulePath, ModuleDefList),
    SuccessFilterFun = fun(#success_result{}) -> true;
                          (_Other) -> false end,
    NonSuccessFilterFun = fun(#success_result{}) -> false;
                             (_Other) -> true end,
    SuccessResults = lists:filter(SuccessFilterFun, Results),
    case lists:filter(NonSuccessFilterFun, Results) of
        [] ->
            io:format("~nExecution successfully completed~n~n", []),
            process_results(SuccessResults);
        NonSuccessResults ->
            io:format("~nExecution finished with some errors~n~n", []),
            process_results(SuccessResults),
            io:format("~nErrors:~n~n", []),
            process_results(NonSuccessResults)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_tasks(ModulePath :: string(), ModuleDefList :: [module_def()]) -> [result_type()].
process_tasks(ModulePath, ModuleDefList) -> process_tasks(ModulePath, ModuleDefList, []).

-spec process_tasks(ModulePath :: string(), ModuleDefList :: [module_def()], Results :: [result_type()]) -> [result_type()].
process_tasks(_ModulePath, [], Results) -> lists:reverse(Results);
process_tasks(ModulePath, [ModuleDef | ModuleDefRest], Results) ->
    TaskResults = process_task(ModulePath, ModuleDef),
    process_tasks(ModulePath, ModuleDefRest, TaskResults ++ Results).

-spec process_task(ModulePath :: string(), ModuleDef :: module_def()) -> [result_type()].
process_task(ModulePath, {ModuleName, AttentionTime, WarningTime, MaxTime}) ->
    numerical_task_executor:process(ModuleName, #time_thresholds{attention = AttentionTime, warning = WarningTime, max = MaxTime}, ModulePath);
process_task(ModulePath, ModuleName) -> process_task(ModulePath, {ModuleName, ?CALC_ATTENTION_TIME, ?CALC_WARNING_TIME, ?CALC_MAX_TIME}).

-spec process_results(Results :: [result_type()]) -> 'ok'.
process_results([]) -> ok;
process_results([#success_result{status = success} | Rest]) -> process_results(Rest);
process_results([#success_result{task_name = ModuleName, input = Input, status = attention, time = Time, thresholds = TimeThresholds} | Rest]) ->
    io:format("ATTENTION: for task ~p: input = ~p, time = ~p ms, attention time = ~p ms, max time = ~p ms~n",
              [ModuleName, Input, Time, TimeThresholds#time_thresholds.attention, TimeThresholds#time_thresholds.max]),
    process_results(Rest);
process_results([#success_result{task_name = ModuleName, input = Input, status = warning, time = Time, thresholds = TimeThresholds} | Rest]) ->
    io:format("WARNING: for task ~p: input = ~p, time = ~p ms, warning time = ~p ms, max time = ~p ms~n",
              [ModuleName, Input, Time, TimeThresholds#time_thresholds.warning, TimeThresholds#time_thresholds.max]),
    process_results(Rest);
process_results([#fail_time_result{task_name = ModuleName, input = Input, time = Time, max_time = MaxTime} | Rest]) ->
    io:format("ERROR: for task ~p: input = ~p, time = ~p ms, max time = ~p ms~n", [ModuleName, Input, Time, MaxTime]),
    process_results(Rest);
process_results([#fail_value_result{task_name = ModuleName, input = Input, expected_value = Expected, actual_value = Actual} | Rest]) ->
    io:format("ERROR: for task ~p: input = ~p, expected value = ~p, actual value = ~p~n", [ModuleName, Input, Expected, Actual]),
    process_results(Rest);
process_results([#fail_exec_result{task_name = ModuleName, input = Input, reason = Reason} | Rest]) ->
    io:format("ERROR: for task ~p: input = ~p, fail reason = ~p~n", [ModuleName, Input, Reason]),
    process_results(Rest).