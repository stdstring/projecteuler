%% @author std-string

%% By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, we form a square number: 1296 = 362.
%% What is remarkable is that, by using the same digital substitutions, the anagram, RACE, also forms a square number: 9216 = 962.
%% We shall call CARE (and RACE) a square anagram word pair and specify further that leading zeroes are not permitted,
%% neither may a different letter have the same digital value as another letter.
%% Using "problem_098.dat", a 16K text file containing nearly two-thousand common English words, find all the square anagram word pairs
%% (a palindromic word is NOT considered to be an anagram of itself).
%% What is the largest square number formed by any member of such a pair?
%% NOTE: All anagrams formed must be contained in the given text file.

-module(problem_098).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(ALPHABET_SIZE, 26).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{"problem_098.dat", 18769}].

prepare_data(ModuleSourceDir, Filename) ->
    WordList = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    split_words(WordList).

solve(WordList) ->
    WordsDict = lists:foldl(fun(Word, Dict) -> dict:update(lists:sort(Word), fun(Value) -> [Word] ++ Value end, [Word], Dict) end, dict:new(), WordList),
    FilteredList = dict:to_list(dict:filter(fun(_Key, Value) -> length(Value) == 2 end, WordsDict)),
    WordPairList = lists:map(fun({_Key, [Word1, Word2]}) -> {Word1, Word2} end, FilteredList),
    UpdateFun = fun({Word1, Word2}, Storage) -> Index = length(Word1) - 1, array:set(Index, [{Word1, Word2}] ++ array:get(Index, Storage), Storage) end,
    WordPairStorage = lists:foldl(UpdateFun, array:new([{default, []}]), WordPairList),
    MaxIndex = array:size(WordPairStorage) - 1,
    MaxRangeValue = numbers:power(10, array:size(WordPairStorage)),
    SquareList = generate_square_list(1, MaxRangeValue - 1, []),
    SquareSet = sets:from_list(SquareList),
    process_number(SquareList, WordPairStorage, MaxIndex, MaxRangeValue div 10, SquareSet).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec split_words(WordList :: string()) -> [string()].
split_words(WordList) ->
    string:tokens(string:join(WordList, ""), [$", $,]).

-spec generate_square_list(Number :: pos_integer(), SquareSup :: pos_integer(), SquareList :: [pos_integer()]) -> [pos_integer()].
generate_square_list(Number, SquareSup, SquareList) when (Number * Number) > SquareSup -> SquareList;
generate_square_list(Number, SquareSup, SquareList) ->
    generate_square_list(Number + 1, SquareSup, [Number * Number] ++ SquareList).

-spec match_word(Word :: string(), Number :: pos_integer()) ->
    {'true', LetterMatchStorage :: array:array(0..9)} | 'false'.
match_word(Word, Number) ->
    LetterMatchStorage = array:new([{size, ?ALPHABET_SIZE}, {fixed, true}, {default, undef}]),
    DigitMatchStorage = array:new([{size, 10}, {fixed, true}, {default, undef}]),
    match_word(Word, numbers:get_digits(Number), LetterMatchStorage, DigitMatchStorage).

-spec match_word(Word :: string(), Digits :: [0..9], LetterMatchStorage :: array:array(0..9), DigitMatchStorage :: array:array(char())) ->
    {'true', LetterMatchStorage :: array:array(0..9)} | 'false'.
match_word([], [], LetterMatchStorage, _DigitMatchStorage) -> {true, LetterMatchStorage};
match_word([Letter | WordRest], [Digit | DigitsRest], LetterMatchStorage, DigitMatchStorage) ->
    LetterMatchValue = array:get(Letter - $A, LetterMatchStorage),
    DigitMatchValue = array:get(Digit, DigitMatchStorage),
    if
        (LetterMatchValue == undef) and (DigitMatchValue == undef) ->
            NewLetterMatchStorage = array:set(Letter - $A, Digit, LetterMatchStorage),
            NewDigitMatchStorage = array:set(Digit, Letter, DigitMatchStorage),
            match_word(WordRest, DigitsRest, NewLetterMatchStorage, NewDigitMatchStorage);
        (LetterMatchValue == Digit) and (DigitMatchValue == Letter) ->
            match_word(WordRest, DigitsRest, LetterMatchStorage, DigitMatchStorage);
        true -> false
    end.

-spec generate_number(Word :: string(), LetterMatchStorage :: array:array(0..9)) -> pos_integer().
generate_number(Word, LetterMatchStorage) ->
    numbers:get_number(lists:map(fun(Letter) -> array:get(Letter - $A, LetterMatchStorage) end, Word)).

-spec check_word(Word :: string(), LetterMatchStorage :: array:array(0..9), SquareSet :: sets:set(pos_integer())) -> boolean().
check_word([FirstLetter | _] = Word, LetterMatchStorage, SquareSet) ->
    case array:get(FirstLetter - $A, LetterMatchStorage) of
        0 -> false;
        _Other -> sets:is_element(generate_number(Word, LetterMatchStorage), SquareSet)
    end.

-spec check_word_pair(Number :: pos_integer(), Word1 :: string(), Word2 :: string(), SquareSet :: sets:set(pos_integer())) -> boolean().
check_word_pair(Number, Word1, Word2, SquareSet) ->
    case match_word(Word1, Number) of
        {true, MatchStorage} -> check_word(Word2, MatchStorage, SquareSet);
        false ->
            case match_word(Word2, Number) of
                {true, MatchStorage} -> check_word(Word1, MatchStorage, SquareSet);
                false -> false
            end
    end.

-spec check_number(Number :: pos_integer(), WordPairs :: [{Word1 :: string(), Word2 :: string()}], SquareSet :: sets:set(pos_integer())) -> boolean().
check_number(_Number, [], _SquareSet) -> false;
check_number(Number, [{Word1, Word2} | WordPairRest], SquareSet) ->
    case check_word_pair(Number, Word1, Word2, SquareSet) of
        false -> check_number(Number, WordPairRest, SquareSet);
        true -> true
    end.

-spec process_number_range(NumberList :: [pos_integer()],
                           MinRangeValue :: non_neg_integer(),
                           WordPairs :: [{Word1 :: string(), Word2 :: string()}],
                           SquareSet :: sets:set(pos_integer())) ->
    {'true', Number :: pos_integer()} | {'false', SquareListRest :: [pos_integer()]}.
process_number_range([Number | _Rest] = SquareListRest, MinRangeValue, _WordPairs, _SquareSet) when Number < MinRangeValue ->
    {false, SquareListRest};
process_number_range([Number | NumbersRest], MinRangeValue, WordPairs, SquareSet) ->
    case check_number(Number, WordPairs, SquareSet) of
        true -> {true, Number};
        false -> process_number_range(NumbersRest, MinRangeValue, WordPairs, SquareSet)
    end.

-spec skip_numbers(NumberList :: [pos_integer()], MinRangeValue :: non_neg_integer()) -> [pos_integer()].
skip_numbers([Number | _Rest]  = SquareListRest, MinRangeValue) when Number < MinRangeValue -> SquareListRest;
skip_numbers([_Number | NumbersRest], MinRangeValue) -> skip_numbers(NumbersRest, MinRangeValue).

-spec process_number(Numbers :: [pos_integer()],
                     WordPairStorage :: array:array([{Word1 :: string(), Word2 :: string()}]),
                     Index :: integer(),
                     MinRangeValue :: non_neg_integer(),
                     SquareSet :: sets:set(pos_integer())) ->
    pos_integer() | no_return().
process_number(_Numbers, _WordPairStorage, -1, 0, _SquareSet) -> error(logic_error);
process_number(Numbers, WordPairStorage, Index, MinRangeValue, SquareSet) ->
    case array:get(Index, WordPairStorage) of
        [] -> process_number(skip_numbers(Numbers, MinRangeValue), WordPairStorage, Index - 1, MinRangeValue div 10, SquareSet);
        WordPairs ->
            case process_number_range(Numbers, MinRangeValue, WordPairs, SquareSet) of
                {false, NumbersRest} -> process_number(NumbersRest, WordPairStorage, Index - 1, MinRangeValue div 10, SquareSet);
                {true, Number} -> Number
            end
    end.