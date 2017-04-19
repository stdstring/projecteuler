%% @author std-string

-module(permutations).
-export([get_permutation/2, get_lexographic_number/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_permutation(LexographicNumber :: non_neg_integer(), Alphabet :: array:array(term())) -> [term()].
get_permutation(LexographicNumber, Alphabet) ->
    AlphabetCount = array:size(Alphabet),
    get_permutation_impl(LexographicNumber, Alphabet, AlphabetCount, []).

-spec get_lexographic_number(Items :: [term()], Alphabet :: array:array(term())) -> non_neg_integer().
get_lexographic_number(Items, Alphabet) ->
    get_lexographic_number_impl(Items, length(Items), Alphabet, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_permutation_impl(LexographicNumber :: non_neg_integer(),
                           AlphabetRest :: array:array(term()),
                           AlphabetRestCount :: non_neg_integer(),
                           Result :: [term()]) -> [term()].
get_permutation_impl(0, _AlphabetRest, 0, Result) -> lists:reverse(Result);
get_permutation_impl(LexographicNumber, AlphabetRest, AlphabetRestCount, Result) ->
    Divider = numbers:factorial(AlphabetRestCount - 1),
    ItemIndex = LexographicNumber div Divider,
    LexographicNumberRest = LexographicNumber rem Divider,
    {Item, NewAlphabetRest} = find_item(AlphabetRest, ItemIndex),
    get_permutation_impl(LexographicNumberRest, NewAlphabetRest, AlphabetRestCount - 1, [Item] ++ Result).

-spec find_item(AlphabetRest :: array:array(term()), ItemIndex :: non_neg_integer()) ->
    {Item :: term(), AlphabetRest :: array:array(term())} | no_return().
find_item(AlphabetRest, ItemIndex) ->
    find_item(AlphabetRest, array:size(AlphabetRest), ItemIndex, 0).

-spec find_item(AlphabetRest :: array:array(term()), AlphabetSize :: pos_integer(), ItemIndex :: non_neg_integer(), Index :: non_neg_integer()) ->
    {Item :: term(), AlphabetRest :: array:array(term())} | no_return().
find_item(_AlphabetRest, AlphabetSize, _ItemIndex, AlphabetSize) -> throw(badarg);
find_item(AlphabetRest, AlphabetSize, 0, Index) ->
    case array:get(Index, AlphabetRest) of
        undef -> find_item(AlphabetRest, AlphabetSize, 0, Index + 1);
        Item -> {Item, array:set(Index, undef, AlphabetRest)}
    end;
find_item(AlphabetRest, AlphabetSize, ItemIndex, Index) ->
    case array:get(Index, AlphabetRest) of
        undef -> find_item(AlphabetRest, AlphabetSize, ItemIndex, Index + 1);
        _Item -> find_item(AlphabetRest, AlphabetSize, ItemIndex - 1, Index + 1)
    end.

-spec get_lexographic_number_impl(Items :: [term()],
                                  ItemsCount :: pos_integer(),
                                  AlphabetRest :: array:array(term()),
                                  Result :: non_neg_integer()) -> non_neg_integer().
get_lexographic_number_impl([], 0, _AlphabetRest, Result) -> Result;
get_lexographic_number_impl([Item | Items], ItemsCount, AlphabetRest, Result) ->
    case find_index(AlphabetRest, Item) of
        {-1, _NewAlphabetRest} -> throw(badarg);
        {ItemIndex, NewAlphabetRest} ->
            NewResult = Result + ItemIndex * numbers:factorial(ItemsCount - 1),
            get_lexographic_number_impl(Items, ItemsCount - 1, NewAlphabetRest, NewResult)
    end.

-spec find_index(AlphabetRest :: array:array(term()),
                 SearchedItem :: term()) -> {SearchedIndex :: non_neg_integer(), AlphabetRest :: array:array(term())}.
find_index(AlphabetRest, SearchedItem) ->
    find_index_impl(AlphabetRest, SearchedItem, 0, 0, array:size(AlphabetRest)).

-spec find_index_impl(AlphabetRest :: array:array(term()),
                      SearchedItem :: term(),
                      SearchedIndex :: non_neg_integer(),
                      Index :: non_neg_integer(),
                      AlphabetCount :: pos_integer()) -> {SearchedIndex :: non_neg_integer(), AlphabetRest :: array:array(term())}.
find_index_impl(AlphabetRest, _SearchedItem, _SearchedIndex, AlphabetCount, AlphabetCount) -> {-1, AlphabetRest};
find_index_impl(AlphabetRest, SearchedItem, SearchedIndex, Index, AlphabetCount) ->
    Item = array:get(Index, AlphabetRest),
    if
        Item == undef -> find_index_impl(AlphabetRest, SearchedItem, SearchedIndex, Index + 1, AlphabetCount);
        Item /= SearchedItem -> find_index_impl(AlphabetRest, SearchedItem, SearchedIndex + 1, Index + 1, AlphabetCount);
        Item == SearchedItem -> {SearchedIndex, array:set(Index, undef, AlphabetRest)}
    end.