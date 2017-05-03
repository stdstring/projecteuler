%% @author std-string

-module(rational_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

add_error_test_() ->
    [{"add({4.0, 3}, {1, 2})", ?_assertError(badarg, rational:add({4.0, 3}, {1, 2}))},
     {"add({4, 3.0}, {1, 2})", ?_assertError(badarg, rational:add({4, 3.0}, {1, 2}))},
     {"add({4, 3}, {1.0, 2})", ?_assertError(badarg, rational:add({4, 3}, {1.0, 2}))},
     {"add({4, 3}, {1, 2.0})", ?_assertError(badarg, rational:add({4.0, 3}, {1, 2.0}))},
     {"add({4, 3, 2}, {1, 2})", ?_assertError(badarg, rational:add({4, 3, 2}, {1, 2}))},
     {"add({4, 3}, {1, 2, 3})", ?_assertError(badarg, rational:add({4, 3}, {1, 2, 3}))},
     {"add(4.0, {1, 2})", ?_assertError(badarg, rational:add(4.0, {1, 2}))},
     {"add(4, {1.0, 2})", ?_assertError(badarg, rational:add(4, {1.0, 2}))},
     {"add(4, {1, 2.0})", ?_assertError(badarg, rational:add(4, {1, 2.0}))},
     {"add({1.0, 2}, 4)", ?_assertError(badarg, rational:add({1.0, 2}, 4))},
     {"add({1, 2.0}, 4)", ?_assertError(badarg, rational:add({1, 2.0}, 4))},
     {"add({1, 2}, 4.0)", ?_assertError(badarg, rational:add({1, 2}, 4.0))},
     {"add(1, 2.0)", ?_assertError(badarg, rational:add(1, 2.0))},
     {"add(1.0, 2)", ?_assertError(badarg, rational:add(1.0, 2))},
     {"add({4, 0}, {1, 2})", ?_assertError(badarg, rational:add({4, 0}, {1, 2}))},
     {"add({4, 3}, {1, 0})", ?_assertError(badarg, rational:add({4, 3}, {1, 0}))},
     {"add(4, {1, 0})", ?_assertError(badarg, rational:add(4, {1, 0}))},
     {"add({1, 0}, 4)", ?_assertError(badarg, rational:add({1, 0}, 4))}].

add_test_() ->
    [{"add({1, 2}, {1, 4})", ?_assertEqual({6, 8}, rational:add({1, 2}, {1, 4}))},
     {"add({2, 4}, {3, 4})", ?_assertEqual({5, 4}, rational:add({2, 4}, {3, 4}))},
     {"add({2, 5}, {2, 3})", ?_assertEqual({16, 15}, rational:add({2, 5}, {2, 3}))},
     {"add({0, 5}, {2, 3})", ?_assertEqual({2, 3}, rational:add({0, 5}, {2, 3}))},
     {"add({2, 3}, {0, 5})", ?_assertEqual({2, 3}, rational:add({2, 3}, {0, 5}))},
     {"add(2, {2, 3})", ?_assertEqual({8, 3}, rational:add(2, {2, 3}))},
     {"add({2, 3}, 1)", ?_assertEqual({5, 3}, rational:add({2, 3}, 1))},
     {"add(0, {2, 3})", ?_assertEqual({2, 3}, rational:add(0, {2, 3}))},
     {"add({2, 3}, 0)", ?_assertEqual({2, 3}, rational:add({2, 3}, 0))},
     {"add(2, 3)", ?_assertEqual(5, rational:add(2, 3))},
     {"add(0, 3)", ?_assertEqual(3, rational:add(0, 3))}].

reverse_error_test_() ->
    [{"reverse({4.0, 3})", ?_assertError(badarg, rational:reverse({4.0, 3}))},
     {"reverse({4, 3.0})", ?_assertError(badarg, rational:reverse({4, 3.0}))},
     {"reverse({4, 0})", ?_assertError(badarg, rational:reverse({4, 0}))},
     {"reverse(4.0)", ?_assertError(badarg, rational:reverse(4.0))},
     {"reverse(0)", ?_assertError(badarg, rational:reverse(0))}].

reverse_test_() ->
    [{"reverse({1, 3})", ?_assertEqual(3, rational:reverse({1, 3}))},
     {"reverse({2, 3})", ?_assertEqual({3, 2}, rational:reverse({2, 3}))},
     {"reverse({4, 6})", ?_assertEqual({6, 4}, rational:reverse({4, 6}))},
     {"reverse(666)", ?_assertEqual({1, 666}, rational:reverse(666))}].

simplify_error_test_() ->
    [{"simplify({4.0, 3})", ?_assertError(badarg, rational:simplify({4.0, 3}))},
     {"simplify({4, 3.0})", ?_assertError(badarg, rational:simplify({4, 3.0}))},
     {"simplify({4, 0})", ?_assertError(badarg, rational:simplify({4, 0}))},
     {"simplify(4.0)", ?_assertError(badarg, rational:simplify(4.0))}].

simplify_test_() ->
    [{"simplify({3, 4})", ?_assertEqual({3, 4}, rational:simplify({3, 4}))},
     {"simplify({4, 3})", ?_assertEqual({4, 3}, rational:simplify({4, 3}))},
     {"simplify({6, 21})", ?_assertEqual({2, 7}, rational:simplify({6, 21}))},
     {"simplify({21, 6})", ?_assertEqual({7, 2}, rational:simplify({21, 6}))},
     {"simplify({2, 4})", ?_assertEqual({1, 2}, rational:simplify({2, 4}))},
     {"simplify({4, 2})", ?_assertEqual(2, rational:simplify({4, 2}))},
     {"simplify(13)", ?_assertEqual(13, rational:simplify(13))}].

%% ====================================================================
%% Internal functions
%% ====================================================================