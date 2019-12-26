%% @author std-string

-module(string_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

format_test_() ->
    [{"format string = \"iddqd = ~p\", values = [666]", ?_assertEqual("iddqd = 666", string_utils:format("iddqd = ~p", [666]))},
     {"format string = \"iddqd = ~s\", values = [\"666\"]", ?_assertEqual("iddqd = 666", string_utils:format("iddqd = ~s", ["666"]))}].

%% ====================================================================
%% Internal functions
%% ====================================================================