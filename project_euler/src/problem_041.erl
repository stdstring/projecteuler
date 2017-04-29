%% @author std-string

%% We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
%% For example, 2143 is a 4-digit pandigital and is also prime. What is the largest n-digit pandigital prime that exists?

-module(problem_041).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type digits_alphabet() :: array:array(numbers:digit()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 7652413}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

%% solution
%% 1) 10-digits pandigital number contains 0..9 digits. Their sum = 45. 45 is divisible by 3 => all 10-digits pandigital numbers is divisable by 3
%% 2) 9-digits pandigital number contains 1..9 digits. Their sum = 45. 45 is divisible by 3 => all 9-digits pandigital numbers is divisable by 3
%% 3) 8-digits pandigital number contains 1..8 digits. Their sum = 36. 36 is divisible by 3 => all 8-digits pandigital numbers is divisable by 3
%% 4) 7-digits pandigital number contains 1..7 digits. Their sum = 28. 28 is not divisible by 3 => all 7-digits pandigital numbers is not divisable by 3
%% So we find max prime pandigital number in 0..7654321 range
-spec solve(PreparedInput :: term()) -> term().
solve(none) ->
    MaxNumber = numbers:get_digits(7654321),
    Alphabet = array:from_list(lists:seq(1, 7)),
    LexographicNumber = permutations:get_lexographic_number(MaxNumber, Alphabet),
    numbers:get_number(process_number(MaxNumber, LexographicNumber, Alphabet)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_number(NumberDigits :: numbers:digits(),
                     LexographicNumber :: non_neg_integer(),
                     Alphabet :: digits_alphabet()) -> numbers:digits().
process_number(NumberDigits, 0, _Alphabet) ->
    Number = numbers:get_number(NumberDigits),
    case number_dividers:is_prime(Number) of
        true -> NumberDigits;
        false -> error(badarg)
    end;
process_number(NumberDigits, LexographicNumber, Alphabet) ->
    Number = numbers:get_number(NumberDigits),
    case number_dividers:is_prime(Number) of
        true -> NumberDigits;
        false ->
            NextLexographicNumber = LexographicNumber - 1,
            NextNumber = permutations:get_permutation(NextLexographicNumber, Alphabet),
            process_number(NextNumber, NextLexographicNumber, Alphabet)
    end.