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
    AttentionFilterFun = fun(#success_result{status = attention}) -> true;
                            (_Other) -> false end,
    WarningFilterFun = fun(#success_result{status = warning}) -> true;
                          (_Other) -> false end,
    NonSuccessFilterFun = fun(#success_result{}) -> false;
                             (_Other) -> true end,
    AttentionResults = lists:filter(AttentionFilterFun, Results),
    WarningResults = lists:filter(WarningFilterFun, Results),
    case lists:filter(NonSuccessFilterFun, Results) of
        [] ->
            io:format("~nExecution successfully completed~n", []),
            process_results("~nATTENTIONS:~n", AttentionResults),
            process_results("~nWARNINGS:~n", WarningResults);
        NonSuccessResults ->
            io:format("~nExecution finished with some errors~n", []),
            process_results("~nATTENTIONS:~n", AttentionResults),
            process_results("~nWARNINGS:~n", WarningResults),
            process_results("~nERRORS:~n", NonSuccessResults)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_tasks(ModulePath :: string(), ModuleDefList :: [module_def()]) -> [result_type()].
process_tasks(ModulePath, ModuleDefList) ->
    DefaultTimeThresholds = #time_thresholds{attention = ?CALC_ATTENTION_TIME, warning = ?CALC_WARNING_TIME, max = ?CALC_MAX_TIME},
    process_tasks(ModulePath, ModuleDefList, DefaultTimeThresholds, []).

-spec process_tasks(ModulePath :: string(),
                    ModuleDefList :: [module_def()],
                    DefaultTimeThresholds :: #time_thresholds{},
                    Results :: [result_type()]) -> [result_type()].
process_tasks(_ModulePath, [], _DefaultTimeThresholds, Results) -> lists:reverse(Results);
process_tasks(ModulePath, [ModuleDef | ModuleDefRest], DefaultTimeThresholds, Results) ->
    TaskResults = process_task(ModulePath, ModuleDef, DefaultTimeThresholds),
    process_tasks(ModulePath, ModuleDefRest, DefaultTimeThresholds, lists:reverse(TaskResults) ++ Results).

-spec process_task(ModulePath :: string(), ModuleDef :: module_def(), DefaultTimeThresholds :: #time_thresholds{}) -> [result_type()].
process_task(ModulePath, {ModuleName, AttentionTime, WarningTime, MaxTime}, _DefaultTimeThresholds) ->
    TimeThresholds = #time_thresholds{attention = AttentionTime, warning = WarningTime, max = MaxTime},
    numerical_task_executor:process(ModuleName, ModulePath, TimeThresholds);
process_task(ModulePath, ModuleName, DefaultTimeThresholds) -> numerical_task_executor:process(ModuleName, ModulePath, DefaultTimeThresholds).

-spec process_results(Header :: string(), Results :: [result_type()]) -> 'ok'.
process_results(_Header, []) -> ok;
process_results(Header, Results) ->
    io:format(Header, []),
    process_results(Results).

-spec process_results(Results :: [result_type()]) -> 'ok'.
process_results([]) -> ok;
process_results([#success_result{status = success} | Rest]) -> process_results(Rest);
process_results([#success_result{task_name = ModuleName, input = Input, status = attention, time = Time, thresholds = TimeThresholds} | Rest]) ->
    io:format("for task ~p: input = ~p, time = ~p ms, attention time = ~p ms, max time = ~p ms~n",
              [ModuleName, Input, Time, TimeThresholds#time_thresholds.attention, TimeThresholds#time_thresholds.max]),
    process_results(Rest);
process_results([#success_result{task_name = ModuleName, input = Input, status = warning, time = Time, thresholds = TimeThresholds} | Rest]) ->
    io:format("for task ~p: input = ~p, time = ~p ms, warning time = ~p ms, max time = ~p ms~n",
              [ModuleName, Input, Time, TimeThresholds#time_thresholds.warning, TimeThresholds#time_thresholds.max]),
    process_results(Rest);
process_results([#fail_time_result{task_name = ModuleName, input = Input, time = Time, max_time = MaxTime} | Rest]) ->
    io:format("for task ~p: input = ~p, time = ~p ms, max time = ~p ms~n", [ModuleName, Input, Time, MaxTime]),
    process_results(Rest);
process_results([#fail_value_result{task_name = ModuleName, input = Input, expected_value = Expected, actual_value = Actual} | Rest]) ->
    io:format("for task ~p: input = ~p, expected value = ~p, actual value = ~p~n", [ModuleName, Input, Expected, Actual]),
    process_results(Rest);
process_results([#fail_exec_result{task_name = ModuleName, input = Input, reason = Reason} | Rest]) ->
    io:format("for task ~p: input = ~p, fail reason = ~p~n", [ModuleName, Input, Reason]),
    process_results(Rest).