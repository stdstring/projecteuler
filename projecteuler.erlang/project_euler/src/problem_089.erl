%% @author std-string

%% For a number written in Roman numerals to be considered valid there are basic rules which must be followed.
%% Even though the rules allow some numbers to be expressed in more than one way there is always a "best" way of writing a particular number.
%% For example, it would appear that there are at least six ways of writing the number sixteen:
%% IIIIIIIIIIIIIIII
%% VIIIIIIIIIII
%% VVIIIIII
%% XIIIIII
%% VVVI
%% XVI
%% However, according to the rules only XIIIIII and XVI are valid, and the last example is considered to be the most efficient, as it uses the least number of numerals.
%% The 11K text file, "problem_089.dat", contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals.
%% Find the number of characters saved by writing each of these in their minimal form.
%% Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.

-module(problem_089).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% M -> 1000
%% CM -> 900
%% D -> 500
%% CD -> 400
%% C -> 100
%% XC -> 90
%% L -> 50
%% XL -> 40
%% X -> 10
%% IX -> 9
%% V -> 5
%% IV -> 4
%% I -> 1

-record(number, {thousands = 0 :: non_neg_integer(),
                 hundreds = 0 :: numbers:digit(),
                 tens = 0 :: numbers:digit(),
                 ones = 0 :: numbers:digit()}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_089.dat", 743}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_strings(filename:join(ModuleSourceDir, Filename)).

-spec solve(PreparedInput :: term()) -> term().
solve(WordList) ->
    lists:foldl(fun(SourceNumber, Sum) -> (length(SourceNumber) - length(simplify_number(SourceNumber))) + Sum end, 0, WordList).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec simplify_number(SourceRoman :: string()) -> string().
simplify_number(SourceRoman) -> generate_roman(parse_source_roman(SourceRoman)).

-spec parse_source_roman(SourceRoman :: string()) -> #number{}.
parse_source_roman(SourceRoman) -> parse_source_roman(SourceRoman, #number{}).

-spec parse_source_roman(SourceRoman :: string(), Number :: #number{}) -> #number{}.
parse_source_roman([], Number) -> normalize_number(Number);
parse_source_roman([$M | Rest], Number) ->
    parse_source_roman(Rest, Number#number{thousands = Number#number.thousands + 1});
parse_source_roman([$C, $M | Rest], Number) ->
    parse_source_roman(Rest, Number#number{hundreds = Number#number.hundreds + 9});
parse_source_roman([$D | Rest], Number) ->
    parse_source_roman(Rest, Number#number{hundreds = Number#number.hundreds + 5});
parse_source_roman([$C, $D | Rest], Number) ->
    parse_source_roman(Rest, Number#number{hundreds = Number#number.hundreds + 4});
parse_source_roman([$C | Rest], Number) ->
    parse_source_roman(Rest, Number#number{hundreds = Number#number.hundreds + 1});
parse_source_roman([$X, $C | Rest], Number) ->
    parse_source_roman(Rest, Number#number{tens = Number#number.tens + 9});
parse_source_roman([$L | Rest], Number) ->
    parse_source_roman(Rest, Number#number{tens = Number#number.tens + 5});
parse_source_roman([$X, $L | Rest], Number) ->
    parse_source_roman(Rest, Number#number{tens = Number#number.tens + 4});
parse_source_roman([$X | Rest], Number) ->
    parse_source_roman(Rest, Number#number{tens = Number#number.tens + 1});
parse_source_roman([$I, $X | Rest], Number) ->
    parse_source_roman(Rest, Number#number{ones = Number#number.ones + 9});
parse_source_roman([$V | Rest], Number) ->
    parse_source_roman(Rest, Number#number{ones = Number#number.ones + 5});
parse_source_roman([$I, $V | Rest], Number) ->
    parse_source_roman(Rest, Number#number{ones = Number#number.ones + 4});
parse_source_roman([$I | Rest], Number) ->
    parse_source_roman(Rest, Number#number{ones = Number#number.ones + 1}).

-spec normalize_number(Number :: #number{}) -> #number{}.
normalize_number(#number{ones = Ones} = Number) when Ones > 9 ->
    normalize_number(Number#number{tens = Number#number.tens + (Ones div 10), ones = Ones rem 10});
normalize_number(#number{tens = Tens} = Number) when Tens > 9 ->
    normalize_number(Number#number{hundreds = Number#number.hundreds + (Tens div 10), tens = Tens rem 10});
normalize_number(#number{hundreds = Hundreds} = Number) when Hundreds > 9 ->
    normalize_number(Number#number{thousands = Number#number.thousands + (Hundreds div 10), hundreds = Hundreds rem 10});
normalize_number(Number) -> Number.

-spec generate_roman(Number :: #number{}) -> string().
generate_roman(#number{thousands = Thousands, hundreds = Hundreds, tens = Tens, ones = Ones}) ->
    generate_roman_thousands(Thousands) ++
    generate_roman_hundreds(Hundreds) ++
    generate_roman_tens(Tens) ++
    generate_roman_ones(Ones).

-spec generate_roman_ones(Ones :: numbers:digit()) -> string().
generate_roman_ones(0) -> "";
generate_roman_ones(1) -> "I";
generate_roman_ones(2) -> "II";
generate_roman_ones(3) -> "III";
generate_roman_ones(4) -> "IV";
generate_roman_ones(5) -> "V";
generate_roman_ones(6) -> "VI";
generate_roman_ones(7) -> "VII";
generate_roman_ones(8) -> "VIII";
generate_roman_ones(9) -> "IX".

-spec generate_roman_tens(Tens :: numbers:digit()) -> string().
generate_roman_tens(0) -> "";
generate_roman_tens(1) -> "X";
generate_roman_tens(2) -> "XX";
generate_roman_tens(3) -> "XXX";
generate_roman_tens(4) -> "XL";
generate_roman_tens(5) -> "L";
generate_roman_tens(6) -> "LX";
generate_roman_tens(7) -> "LXX";
generate_roman_tens(8) -> "LXXX";
generate_roman_tens(9) -> "XC".

-spec generate_roman_hundreds(Hundreds :: numbers:digit()) -> string().
generate_roman_hundreds(0) -> "";
generate_roman_hundreds(1) -> "C";
generate_roman_hundreds(2) -> "CC";
generate_roman_hundreds(3) -> "CCC";
generate_roman_hundreds(4) -> "CD";
generate_roman_hundreds(5) -> "D";
generate_roman_hundreds(6) -> "DC";
generate_roman_hundreds(7) -> "DCC";
generate_roman_hundreds(8) -> "DCCC";
generate_roman_hundreds(9) -> "CM".

-spec generate_roman_thousands(Number :: non_neg_integer()) -> string().
generate_roman_thousands(Number) -> string:copies("M", Number).