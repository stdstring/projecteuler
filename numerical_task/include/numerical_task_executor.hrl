-define(DEFAULT_MAX_TIME, 5 * 1000 * 1000).

-record(success_result, {task_name = undefined :: 'undefined' | atom(),
                         input = undefined :: 'undefined' | term(),
                         current_time = 0 :: pos_integer(),
                         max_time =  0 :: pos_integer()}).
-record(fail_time_result, {task_name = undefined :: 'undefined' | atom(),
                           input = undefined :: 'undefined' | term(),
                           current_time = 0 :: pos_integer(),
                           max_time =  0 :: pos_integer()}).
-record(fail_value_result, {task_name = undefined :: 'undefined' | atom(),
                            input = undefined :: 'undefined' | term(),
                            expected_value = undefined :: 'undefined' | term(),
                            actual_value = undefined :: 'undefined' | term()}).
-record(fail_exec_result, {task_name = undefined :: 'undefined' | atom(),
                           input = undefined :: 'undefined' | term(),
                           reason = undefined :: 'undefined' | term()}).