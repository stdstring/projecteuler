%% @author std-string

-module(permutations_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ALPHABET_09, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]).

%% ====================================================================
%% Test functions
%% ====================================================================

get_permutation_test_() ->
    [{"get 0 permutation for 012", ?_assertEqual([0, 1, 2], get_permutation(0, [0, 1, 2]))},
     {"get 1 permutation for 012", ?_assertEqual([0, 2, 1], get_permutation(1, [0, 1, 2]))},
     {"get 4 permutation for 012", ?_assertEqual([2, 0, 1], get_permutation(4, [0, 1, 2]))},
     {"get 0 permutation for 0123456789", ?_assertEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], get_permutation(0, ?ALPHABET_09))},
     {"get 999999 permutation for 0123456789", ?_assertEqual([2, 7, 8, 3, 9, 1, 5, 4, 6, 0], get_permutation(999999, ?ALPHABET_09))},
     {"get 0 permutation for abc", ?_assertEqual([a, b, c], get_permutation(0, [a, b, c]))},
     {"get 1 permutation for abc", ?_assertEqual([a, c, b], get_permutation(1, [a, b, c]))},
     {"get 4 permutation for abc", ?_assertEqual([c, a, b], get_permutation(4, [a, b, c]))},
     {"try get 1000 permutation for 012", ?_assertError(badarg, get_permutation(1000, [0, 1, 2]))}].

get_lexicographical_number_test_() ->
    [{"get lexicographical number for 012", ?_assertEqual(0, get_lexicographical_number([0, 1, 2], [0, 1, 2]))},
     {"get lexicographical number for 021", ?_assertEqual(1, get_lexicographical_number([0, 2, 1], [0, 1, 2]))},
     {"get lexicographical number for 201", ?_assertEqual(4, get_lexicographical_number([2, 0, 1], [0, 1, 2]))},
     {"get lexicographical number for 0123456789", ?_assertEqual(0, get_lexicographical_number([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], ?ALPHABET_09))},
     {"get lexicographical number for 2783915460", ?_assertEqual(999999, get_lexicographical_number([2, 7, 8, 3, 9, 1, 5, 4, 6, 0], ?ALPHABET_09))},
     {"get lexicographical number for abc", ?_assertEqual(0, get_lexicographical_number([a, b, c], [a, b, c]))},
     {"get lexicographical number for acb", ?_assertEqual(1, get_lexicographical_number([a, c, b], [a, b, c]))},
     {"get lexicographical number for cab", ?_assertEqual(4, get_lexicographical_number([c, a, b], [a, b, c]))},
     {"try get lexicographical number for 002", ?_assertError(badarg, get_lexicographical_number([0, 0, 2], [0, 1, 2]))}].

get_lexicographical_number_sup_test_() ->
    [{"get lexographic number supremum for 1-item alphabet", ?_assertEqual(1, get_lexicographical_number_sup([1]))},
     {"get lexographic number supremum for another 1-item alphabet", ?_assertEqual(1, get_lexicographical_number_sup([a]))},
     {"get lexographic number supremum for 2-item alphabet", ?_assertEqual(2, get_lexicographical_number_sup([1, 2]))},
     {"get lexographic number supremum for another 2-item alphabet", ?_assertEqual(2, get_lexicographical_number_sup([a, b]))},
     {"get lexographic number supremum for 3-item alphabet", ?_assertEqual(6, get_lexicographical_number_sup([1, 2, 3]))},
     {"get lexographic number supremum for another 3-item alphabet", ?_assertEqual(6, get_lexicographical_number_sup([a, b, c]))},
     {"get lexographic number supremum for 4-item alphabet", ?_assertEqual(24, get_lexicographical_number_sup([1, 2, 3, 4]))},
     {"get lexographic number supremum for another 4-item alphabet", ?_assertEqual(24, get_lexicographical_number_sup([a, b, c, d]))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_permutation(LexicographicalNumber :: non_neg_integer(), Alphabet :: [permutations:alphabet_item()]) -> [permutations:alphabet_item()].
get_permutation(LexicographicalNumber, Alphabet) -> permutations:get_permutation(LexicographicalNumber, array:from_list(Alphabet)).

-spec get_lexicographical_number(Items :: [permutations:alphabet_item()], Alphabet :: [permutations:alphabet_item()]) -> non_neg_integer().
get_lexicographical_number(Items, Alphabet) -> permutations:get_lexicographical_number(Items, array:from_list(Alphabet)).

-spec get_lexicographical_number_sup(Alphabet :: [permutations:alphabet_item()]) -> pos_integer().
get_lexicographical_number_sup(Alphabet) -> permutations:get_lexicographical_number_sup(array:from_list(Alphabet)).