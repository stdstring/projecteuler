%% @author std-string

-module(compare_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

compare_asc_test_() ->
    [{"compare 1 and 2", ?_assertEqual(left, compare:compare_asc(1, 2))},
     {"compare 1 and 1", ?_assertEqual(equal, compare:compare_asc(1, 1))},
     {"compare 2 and 1", ?_assertEqual(right, compare:compare_asc(2, 1))},
     {"compare 1 and 1.0", ?_assertEqual(equal, compare:compare_asc(1, 1.0))},
     {"compare \"aab\" and \"abb\"", ?_assertEqual(left, compare:compare_asc("aab", "abb"))},
     {"compare \"aab\" and \"aab\"", ?_assertEqual(equal, compare:compare_asc("aab", "aab"))},
     {"compare \"abb\" and \"aab\"", ?_assertEqual(right, compare:compare_asc("abb", "aab"))},
     {"compare 1 and iddqd", ?_assertEqual(left, compare:compare_asc(1, iddqd))},
     {"compare iddqd and 1", ?_assertEqual(right, compare:compare_asc(iddqd, 1))}].

compare_desc_test_() ->
    [{"compare 1 and 2", ?_assertEqual(right, compare:compare_desc(1, 2))},
     {"compare 1 and 1", ?_assertEqual(equal, compare:compare_desc(1, 1))},
     {"compare 2 and 1", ?_assertEqual(left, compare:compare_desc(2, 1))},
     {"compare 1 and 1.0", ?_assertEqual(equal, compare:compare_desc(1, 1.0))},
     {"compare \"aab\" and \"abb\"", ?_assertEqual(right, compare:compare_desc("aab", "abb"))},
     {"compare \"aab\" and \"aab\"", ?_assertEqual(equal, compare:compare_desc("aab", "aab"))},
     {"compare \"abb\" and \"aab\"", ?_assertEqual(left, compare:compare_desc("abb", "aab"))},
     {"compare 1 and iddqd", ?_assertEqual(right, compare:compare_desc(1, iddqd))},
     {"compare iddqd and 1", ?_assertEqual(left, compare:compare_desc(iddqd, 1))}].

%% ====================================================================
%% Internal functions
%% ====================================================================