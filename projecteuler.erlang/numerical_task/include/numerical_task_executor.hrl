%% @author std-string

%% in milliseconds
-define(CALC_ATTENTION_TIME, 2 * 1000).
%% in milliseconds
-define(CALC_WARNING_TIME, 30 * 1000).
%% in milliseconds
-define(CALC_MAX_TIME, 60 * 1000).

-type success_status() :: 'success' | 'warning' | 'attention'.

-record(time_thresholds, {attention = 0 :: pos_integer(), warning = 0 :: pos_integer(), max = 0 :: pos_integer()}).

-record(success_result, {task_name = undefined :: 'undefined' | atom(),
                         input = undefined :: 'undefined' | term(),
                         status = success :: success_status(),
                         time = 0 :: pos_integer(),
                         thresholds = #time_thresholds{} :: #time_thresholds{}}).
-record(fail_time_result, {task_name = undefined :: 'undefined' | atom(),
                           input = undefined :: 'undefined' | term(),
                           time = 0 :: pos_integer(),
                           max_time =  0 :: pos_integer()}).
-record(fail_value_result, {task_name = undefined :: 'undefined' | atom(),
                            input = undefined :: 'undefined' | term(),
                            expected_value = undefined :: 'undefined' | term(),
                            actual_value = undefined :: 'undefined' | term()}).
-record(fail_exec_result, {task_name = undefined :: 'undefined' | atom(),
                           input = undefined :: 'undefined' | term(),
                           reason = undefined :: 'undefined' | term()}).

-type result_type() :: #success_result{} | #fail_time_result{} | #fail_value_result{} | #fail_exec_result{}.