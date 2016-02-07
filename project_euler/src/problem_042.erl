%% The n-th term of the sequence of triangle numbers is given by, tn = (1/2)*n*(n+1); so the first ten triangle numbers are:
%% 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
%% By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value.
%% For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.
%% Using words.txt, a 16K text file containing nearly two-thousand common English words, how many are triangle words?

-module(problem_042).

-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

get_check_data() ->
    [{"problem_042.dat", 162}].

prepare_data(ModuleSourceDir, Filename) ->
    StringList = load_utils:read_strings(filename:join(ModuleSourceDir, Filename)),
    Words = string:join(StringList, ""),
    lists:map(fun(Str) -> string:strip(Str, both, $") end, string:tokens(Words, ",")).

solve(WordList) ->
    WordValueList = lists:map(fun(Word) -> calc_word_value(Word) end, WordList),
    TriangleNumberSet = generate_triangle_numbers(100),
    TriangleValueList = lists:filter(fun(Value) -> sets:is_element(Value, TriangleNumberSet) end, WordValueList),
    length(TriangleValueList).

generate_triangle_numbers(MaxNumber) ->
    sets:from_list(lists:map(fun(Number) -> Number * (Number - 1) div 2 end, lists:seq(1, MaxNumber))).

calc_word_value(Word) ->
    lists:sum(lists:map(fun(Char) -> Char - $A + 1 end, string:to_upper(Word))).