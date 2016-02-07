-module(numerical_task_list_executor).

-include("numerical_task_executor.hrl").

-export([process/1]).

-spec process(ConfigFilename :: string()) -> 'ok'.
process(ConfigFilename) ->
    {ModulePath, AdditionalPathList, ModuleDefList} = load_utils:read_erlang_term(filename:absname(ConfigFilename)),
    code:add_patha(filename:absname(ModulePath)),
    lists:foreach(fun(Path) -> code:add_patha(filename:absname(Path)) end, AdditionalPathList),
    Results = process_tasks(ModulePath, ModuleDefList),
    ResultFilterFun = fun(#success_result{}) -> false;
                         (_Other) -> true end,
    NonSuccessResults = lists:filter(ResultFilterFun, Results),
    case NonSuccessResults of
        [] -> io:format("Execution successfully completed~n", []);
        _Other ->
            io:format("~nResult:~n", []),
            process_results(Results)
    end,
    ok.

-spec process_tasks(ModulePath :: string(), ModuleDefList :: [{ModuleName :: atom(), MaxTime :: pos_integer()} | atom()]) ->
    [#success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}].
process_tasks(ModulePath, ModuleDefList) ->
    process_tasks(ModulePath, ModuleDefList, []).

-spec process_tasks(ModulePath :: string(),
                    ModuleDefList :: [{ModuleName :: atom(), MaxTime :: pos_integer()} | atom()],
                    Results :: [#success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}]) ->
    [#success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}].
process_tasks(_ModulePath, [], Results) -> lists:reverse(Results);
process_tasks(ModulePath, [ModuleDef | ModuleDefRest], Results) ->
    TaskResults = process_task(ModulePath, ModuleDef),
    process_tasks(ModulePath, ModuleDefRest, TaskResults ++ Results).

-spec process_task(ModulePath :: string(), ModuleDef :: {ModuleName :: atom(), MaxTime :: pos_integer()} | atom()) ->
    [#success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}].
process_task(ModulePath, {ModuleName, MaxTime}) ->
    numerical_task_executor:process(ModuleName, MaxTime, ModulePath);
process_task(ModulePath, ModuleName) ->
    process_task(ModulePath, {ModuleName,?DEFAULT_MAX_TIME}).

-spec process_results(Results :: [#success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}]) -> 'ok'.
process_results([]) -> ok;
process_results([#success_result{} | Rest]) -> process_results(Rest);
process_results([#fail_time_result{task_name = ModuleName, input = Input, current_time = Time, max_time = MaxTime} | Rest]) ->
    io:format("For task ~p: input = ~p, time = ~p, max time = ~p~n", [ModuleName, Input, Time, MaxTime]),
    process_results(Rest);
process_results([#fail_value_result{task_name = ModuleName, input = Input, expected_value = Expected, actual_value = Actual} | Rest]) ->
    io:format("For task ~p: input = ~p, expected value = ~p, actual value = ~p~n", [ModuleName, Input, Expected, Actual]),
    process_results(Rest);
process_results([#fail_exec_result{task_name = ModuleName, input = Input, reason = Reason} | Rest]) ->
    io:format("For task ~p: input = ~p, fail reason = ~p~n", [ModuleName, Input, Reason]),
    process_results(Rest).