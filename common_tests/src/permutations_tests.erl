%% @author std-string

-module(permutations_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ALPHABET_09, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]).

%% ====================================================================
%% Test functions
%% ====================================================================

get_permutation_test_() ->
    [{"get 0 permutation for 012", ?_assertEqual([0, 1, 2], permutations:get_permutation(0, array:from_list([0, 1, 2])))},
     {"get 1 permutation for 012", ?_assertEqual([0, 2, 1], permutations:get_permutation(1, array:from_list([0, 1, 2])))},
     {"get 4 permutation for 012", ?_assertEqual([2, 0, 1], permutations:get_permutation(4, array:from_list([0, 1, 2])))},
     {"get 0 permutation for 0123456789", ?_assertEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], permutations:get_permutation(0, array:from_list(?ALPHABET_09)))},
     {"get 999999 permutation for 0123456789", ?_assertEqual([2, 7, 8, 3, 9, 1, 5, 4, 6, 0], permutations:get_permutation(999999, array:from_list(?ALPHABET_09)))},
     {"get 0 permutation for abc", ?_assertEqual([a, b, c], permutations:get_permutation(0, array:from_list([a, b, c])))},
     {"get 1 permutation for abc", ?_assertEqual([a, c, b], permutations:get_permutation(1, array:from_list([a, b, c])))},
     {"get 4 permutation for abc", ?_assertEqual([c, a, b], permutations:get_permutation(4, array:from_list([a, b, c])))},
     {"try get 1000 permutation for 012", ?_assertThrow(badarg, permutations:get_permutation(1000, array:from_list([0, 1, 2])))}].

get_lexographic_number_test_() ->
    [{"get lexographic number for 012", ?_assertEqual(0, permutations:get_lexographic_number([0, 1, 2], array:from_list([0, 1, 2])))},
     {"get lexographic number for 021", ?_assertEqual(1, permutations:get_lexographic_number([0, 2, 1], array:from_list([0, 1, 2])))},
     {"get lexographic number for 201", ?_assertEqual(4, permutations:get_lexographic_number([2, 0, 1], array:from_list([0, 1, 2])))},
     {"get lexographic number for 0123456789", ?_assertEqual(0, permutations:get_lexographic_number([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], array:from_list(?ALPHABET_09)))},
     {"get lexographic number for 2783915460", ?_assertEqual(999999, permutations:get_lexographic_number([2, 7, 8, 3, 9, 1, 5, 4, 6, 0], array:from_list(?ALPHABET_09)))},
     {"get lexographic number for abc", ?_assertEqual(0, permutations:get_lexographic_number([a, b, c], array:from_list([a, b, c])))},
     {"get lexographic number for acb", ?_assertEqual(1, permutations:get_lexographic_number([a, c, b], array:from_list([a, b, c])))},
     {"get lexographic number for cab", ?_assertEqual(4, permutations:get_lexographic_number([c, a, b], array:from_list([a, b, c])))},
     {"try get lexographic number for 002", ?_assertThrow(badarg, permutations:get_lexographic_number([0, 0, 2], array:from_list([0, 1, 2])))}].

%% ====================================================================
%% Internal functions
%% ====================================================================