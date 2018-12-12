%% @author std-string

-module(permutations).
-export([get_permutation/2, get_lexicographical_number/2, get_lexicographical_number_sup/1]).

-type alphabet_item() :: term().
-type alphabet() :: array:array(alphabet_item()).
-type search_item_result() :: {Item :: alphabet_item(), AlphabetRest :: alphabet()} | no_return().
-type search_index_result() :: {SearchedIndex :: non_neg_integer(), AlphabetRest :: alphabet()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_permutation(LexicographicalNumber :: non_neg_integer(), Alphabet :: alphabet()) -> [alphabet_item()].
get_permutation(LexicographicalNumber, Alphabet) ->
    AlphabetCount = array:size(Alphabet),
    get_permutation_impl(LexicographicalNumber, Alphabet, AlphabetCount, []).

-spec get_lexicographical_number(Items :: [alphabet_item()], Alphabet :: alphabet()) -> non_neg_integer().
get_lexicographical_number(Items, Alphabet) ->
    get_lexicographical_number_impl(Items, length(Items), Alphabet, 0).

-spec get_lexicographical_number_sup(Alphabet :: alphabet()) -> pos_integer().
get_lexicographical_number_sup(Alphabet) -> numbers:factorial(array:size(Alphabet)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_permutation_impl(LexicographicalNumber :: non_neg_integer(),
                           AlphabetRest :: alphabet(),
                           AlphabetRestCount :: non_neg_integer(),
                           Result :: [alphabet_item()]) -> [alphabet_item()].
get_permutation_impl(0, _AlphabetRest, 0, Result) -> lists:reverse(Result);
get_permutation_impl(LexicographicalNumber, AlphabetRest, AlphabetRestCount, Result) ->
    Divider = numbers:factorial(AlphabetRestCount - 1),
    ItemIndex = LexicographicalNumber div Divider,
    LexicographicalNumberRest = LexicographicalNumber rem Divider,
    {Item, NewAlphabetRest} = find_item(AlphabetRest, ItemIndex),
    get_permutation_impl(LexicographicalNumberRest, NewAlphabetRest, AlphabetRestCount - 1, [Item] ++ Result).

-spec find_item(AlphabetRest :: alphabet(), ItemIndex :: non_neg_integer()) -> search_item_result().
find_item(AlphabetRest, ItemIndex) -> find_item(AlphabetRest, array:size(AlphabetRest), ItemIndex, 0).

-spec find_item(AlphabetRest :: alphabet(),
                AlphabetSize :: pos_integer(),
                ItemIndex :: non_neg_integer(),
                Index :: non_neg_integer()) -> search_item_result().
find_item(_AlphabetRest, AlphabetSize, _ItemIndex, AlphabetSize) -> error(badarg);
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

-spec get_lexicographical_number_impl(Items :: [alphabet_item()],
                                      ItemsCount :: pos_integer(),
                                      AlphabetRest :: alphabet(),
                                      Result :: non_neg_integer()) -> non_neg_integer().
get_lexicographical_number_impl([], 0, _AlphabetRest, Result) -> Result;
get_lexicographical_number_impl([Item | Items], ItemsCount, AlphabetRest, Result) ->
    case find_index(AlphabetRest, Item) of
        {-1, _NewAlphabetRest} -> error(badarg);
        {ItemIndex, NewAlphabetRest} ->
            NewResult = Result + ItemIndex * numbers:factorial(ItemsCount - 1),
            get_lexicographical_number_impl(Items, ItemsCount - 1, NewAlphabetRest, NewResult)
    end.

-spec find_index(AlphabetRest :: alphabet(), SearchedItem :: alphabet_item()) -> search_index_result().
find_index(AlphabetRest, SearchedItem) ->
    find_index_impl(AlphabetRest, SearchedItem, 0, 0, array:size(AlphabetRest)).

-spec find_index_impl(AlphabetRest :: alphabet(),
                      SearchedItem :: alphabet_item(),
                      SearchedIndex :: non_neg_integer(),
                      Index :: non_neg_integer(),
                      AlphabetCount :: pos_integer()) -> search_index_result().
find_index_impl(AlphabetRest, _SearchedItem, _SearchedIndex, AlphabetCount, AlphabetCount) -> {-1, AlphabetRest};
find_index_impl(AlphabetRest, SearchedItem, SearchedIndex, Index, AlphabetCount) ->
    Item = array:get(Index, AlphabetRest),
    if
        Item == undef -> find_index_impl(AlphabetRest, SearchedItem, SearchedIndex, Index + 1, AlphabetCount);
        Item /= SearchedItem -> find_index_impl(AlphabetRest, SearchedItem, SearchedIndex + 1, Index + 1, AlphabetCount);
        Item == SearchedItem -> {SearchedIndex, array:set(Index, undef, AlphabetRest)}
    end.