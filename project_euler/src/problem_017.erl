%% @author std-string

%% If the numbers 1 to 5 are written out in words: one, two, three, four, five,
%% then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
%% If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used? 
%% NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters
%% and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

-module(problem_017).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MAX_POSSIBLE_NUMBER, 1000).

%% TODO (std_string) : think about creation module for word processing
-type word() :: string().
-type words_array() :: array:array(word()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{5, 19}, {1000, 21124}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) -> solve_impl(MaxNumber).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec solve_impl(MaxNumber :: pos_integer()) -> pos_integer().
solve_impl(MaxNumber) when MaxNumber > ?MAX_POSSIBLE_NUMBER ->  erlang:error(badarg);
solve_impl(MaxNumber) ->
    From0To9 = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"],
    From10To19 = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"],
    OtherTens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"],
    parse_numbers(MaxNumber, array:from_list(From0To9), array:from_list(From10To19), array:from_list(OtherTens)).

-spec parse_numbers(MaxNumber :: pos_integer(),
                    From0To9 :: words_array(),
                    From10To19 :: words_array(),
                    OtherTens :: words_array()) -> pos_integer().
parse_numbers(MaxNumber, From0To9, From10To19, OtherTens) ->
    parse_numbers(MaxNumber, 1, 0, From0To9, From10To19, OtherTens).

-spec parse_numbers(MaxNumber :: pos_integer(),
                    CurrentNumber :: pos_integer(),
                    LetterCount :: non_neg_integer(),
                    From0To9 :: words_array(),
                    From10To19 :: words_array(),
                    OtherTens :: words_array()) -> pos_integer().
parse_numbers(MaxNumber, CurrentNumber, LetterCount, _From0To9, _From10To19, _OtherTens) when CurrentNumber > MaxNumber -> LetterCount;
parse_numbers(MaxNumber, CurrentNumber, LetterCount, From0To9, From10To19, OtherTens) ->
    CurrentCount = parse_number(CurrentNumber, From0To9, From10To19, OtherTens),
    parse_numbers(MaxNumber, CurrentNumber + 1, LetterCount + CurrentCount, From0To9, From10To19, OtherTens).

-spec parse_number(Number :: pos_integer(),
                   From0To9 :: words_array(),
                   From10To19 :: words_array(),
                   OtherTens :: words_array()) -> pos_integer().
parse_number(1000, _From0To9, _From10To19, _OtherTens) -> length("one" ++ "thousand");
parse_number(Number, From0To9, From10To19, OtherTens) when Number >= 100 ->
        LessHundred = less_hundred(Number rem 100, From0To9, From10To19, OtherTens),
        if
            LessHundred == 0 -> length(array:get(Number div 100, From0To9)) + length("hundred");
            LessHundred /= 0 -> length(array:get(Number div 100, From0To9)) + length("hundred" ++ "and") + LessHundred
        end;
parse_number(Number, From0To9, From10To19, OtherTens) ->
    less_hundred(Number rem 100, From0To9, From10To19, OtherTens).

-spec less_hundred(Number :: non_neg_integer(),
                   From0To9 :: words_array(),
                   From10To19 :: words_array(),
                   OtherTens :: words_array()) -> non_neg_integer().
less_hundred(Number, _TensFrom0To9, _From10To19, _OtherTens) when Number >= 100 -> erlang:error(badarg);
less_hundred(Number, From0To9, _From10To19, _OtherTens) when Number =< 9 ->
    length(array:get(Number, From0To9));
less_hundred(Number, _From0To9, From10To19, _OtherTens) when (Number > 9), (Number < 20) ->
    length(array:get(Number - 10, From10To19));
less_hundred(Number, From0To9, _From10To19, OtherTens) ->
    length(array:get((Number div 10) - 2, OtherTens)) + length(array:get(Number rem 10, From0To9)).