%% @author std-string

-module(collections_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

get_circular_shift_test_() ->
    [{"get_circular_shift for []", ?_assertEqual([], collections:get_circular_shift([]))},
     {"get_circular_shift for [1]", ?_assertEqual([1], collections:get_circular_shift([1]))},
     {"get_circular_shift for [1, 2, 3]", ?_assertEqual([2, 3, 1], collections:get_circular_shift([1, 2, 3]))}].

get_all_circular_shift_test_() ->
    [{"get_all_circular_shift for []", ?_assertEqual([[]], collections:get_all_circular_shift([]))},
     {"get_all_circular_shift for [1]", ?_assertEqual([[1]], collections:get_all_circular_shift([1]))},
     {"get_all_circular_shift for [1, 2]", ?_assertEqual([[1, 2], [2, 1]], collections:get_all_circular_shift([1, 2]))},
     {"get_all_circular_shift for [1, 2, 3]", ?_assertEqual([[1, 2, 3], [2, 3, 1], [3, 1, 2]], collections:get_all_circular_shift([1, 2, 3]))}].

%% ====================================================================
%% Internal functions
%% ====================================================================