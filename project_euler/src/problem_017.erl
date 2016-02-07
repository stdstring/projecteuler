%% If the numbers 1 to 5 are written out in words: one, two, three, four, five,
%% then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
%% If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used? 
%% NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters
%% and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

-module(problem_017).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MAX_POSSIBLE_NUMBER, 1000).

get_check_data() ->
    [{5, 19}, {1000, 21124}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) -> solve_impl(MaxNumber).

solve_impl(MaxNumber) when MaxNumber > ?MAX_POSSIBLE_NUMBER ->  erlang:error(badarg);
solve_impl(MaxNumber) ->
    From0To9 = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"],
    From10To19 = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"],
    OtherTens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"],
    parse_numbers(MaxNumber, From0To9, From10To19, OtherTens).

parse_numbers(MaxNumber, From0To9, From10To19, OtherTens) ->
    parse_numbers(MaxNumber, 1, 0, From0To9, From10To19, OtherTens).

parse_numbers(MaxNumber, CurrentNumber, LetterCount, _From0To9, _From10To19, _OtherTens) when CurrentNumber > MaxNumber -> LetterCount;
parse_numbers(MaxNumber, CurrentNumber, LetterCount, From0To9, From10To19, OtherTens) ->
    parse_numbers(MaxNumber, CurrentNumber+1, LetterCount+parse_number(CurrentNumber, From0To9, From10To19, OtherTens), From0To9, From10To19, OtherTens).

parse_number(1000, _From0To9, _From10To19, _OtherTens) -> length("one" ++ "thousand");
parse_number(Number, From0To9, From10To19, OtherTens) when Number >= 100 ->
        LessHundred = less_hundred(Number rem 100, From0To9, From10To19, OtherTens),
        if
            LessHundred == 0 -> length(lists:nth((Number div 100) + 1, From0To9)) + length("hundred");
            LessHundred /= 0 -> length(lists:nth((Number div 100) + 1, From0To9)) + length("hundred" ++ "and") + LessHundred
        end;
parse_number(Number, From0To9, From10To19, OtherTens) ->
    less_hundred(Number rem 100, From0To9, From10To19, OtherTens).

less_hundred(Number, _TensFrom0To9, _From10To19, _OtherTens) when Number >= 100 -> erlang:error(badarg);
less_hundred(Number, From0To9, _From10To19, _OtherTens) when Number =< 9 ->
    length(lists:nth(Number + 1, From0To9));
less_hundred(Number, _From0To9, From10To19, _OtherTens) when (Number > 9) and (Number < 20) ->
    length(lists:nth(Number - 9, From10To19));
less_hundred(Number, From0To9, _From10To19, OtherTens) ->
    length(lists:nth((Number div 10)-1, OtherTens)) + length(lists:nth((Number rem 10) + 1, From0To9)).