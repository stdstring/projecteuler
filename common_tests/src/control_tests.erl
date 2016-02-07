-module(control_tests).

-include_lib("eunit/include/eunit.hrl").

for_count_test_() ->
    CollectFun = fun(Index, Acc) -> Acc ++ [Index] end,
    [{"collect indices for 0 count", ?_assertEqual([], control:for(0, [], CollectFun))},
     {"collect indices for 1 count", ?_assertEqual([0], control:for(1, [], CollectFun))},
     {"collect indices for 2 count", ?_assertEqual([0, 1], control:for(2, [], CollectFun))},
     {"collect indices for 3 count", ?_assertEqual([0, 1, 2], control:for(3, [], CollectFun))}].

for_from_to_test_() ->
    CollectFun = fun(Index, Acc) -> Acc ++ [Index] end,
    [{"collect indices from 2 to 0", ?_assertEqual([], control:for(2, 0, [], CollectFun))},
     {"collect indices from 2 to 2", ?_assertEqual([2], control:for(2, 2, [], CollectFun))},
     {"collect indices from 2 to 3", ?_assertEqual([2, 3], control:for(2, 3, [], CollectFun))},
     {"collect indices from 2 to 4", ?_assertEqual([2, 3, 4], control:for(2, 4, [], CollectFun))}].

for_from_to_step_test_() ->
    CollectFun = fun(Index, Acc) -> Acc ++ [Index] end,
    [{"collect indices from 2 to 0 step 1", ?_assertEqual([], control:for(2, 0, 1, [], CollectFun))},
     {"collect indices from 2 to 0 step 3", ?_assertEqual([], control:for(2, 0, 3, [], CollectFun))},
     {"collect indices from 2 to 2 step 1", ?_assertEqual([2], control:for(2, 2, 1, [], CollectFun))},
     {"collect indices from 2 to 2 step 3", ?_assertEqual([2], control:for(2, 2, 3, [], CollectFun))},
     {"collect indices from 2 to 3 step 1", ?_assertEqual([2, 3], control:for(2, 3, 1, [], CollectFun))},
     {"collect indices from 2 to 3 step 3", ?_assertEqual([2], control:for(2, 3, 3, [], CollectFun))},
     {"collect indices from 2 to 4 step 1", ?_assertEqual([2, 3, 4], control:for(2, 4, 1, [], CollectFun))},
     {"collect indices from 2 to 4 step 3", ?_assertEqual([2], control:for(2, 4, 3, [], CollectFun))},
     {"collect indices from 2 to 6 step 3", ?_assertEqual([2, 5], control:for(2, 6, 3, [], CollectFun))}].