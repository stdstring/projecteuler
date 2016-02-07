%% The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
%% which is correct, is obtained by cancelling the 9s.
%% We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
%% There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
%% Find these four fractions.

-module(problem_033).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(START_NUMBER, 11).
-define(MAX_NUMBER, 50).

get_check_data() ->
    [{none, [{1, 4}, {1, 5}, {2, 5}, {4, 8}]}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) ->
    lists:reverse(process_number(?START_NUMBER, [])).

process_number(?MAX_NUMBER, Result) -> Result;
process_number(Number, Result) when Number rem 10 == 0 -> process_number(Number + 1, Result);
process_number(Number, Result) ->
    case check_number(Number) of
        {true, Numerator, Denominator} -> process_number(Number + 1, [{Numerator, Denominator}] ++ Result);
        false -> process_number(Number + 1, Result)
    end.

check_number(Number) ->
    NumberDigits = numbers:get_digits(Number),
    check_number(Number, NumberDigits, Number + 1).

check_number(_Number, _NumberDigits, OtherNumber) when OtherNumber >= 100 -> false;
check_number(Number, NumberDigits, OtherNumber) ->
    OtherNumberDigits = numbers:get_digits(OtherNumber),
    case try_simplify(NumberDigits, OtherNumberDigits) of
        {true, Digit1, Digit2} ->
            case check_fraction(Number, OtherNumber, Digit1, Digit2) of
                true -> {true, Digit1, Digit2};
                false -> check_number(Number, NumberDigits, OtherNumber + 1)
            end;
        false -> check_number(Number, NumberDigits, OtherNumber + 1)
    end.

try_simplify([Digit, Digit2], [Digit3, Digit]) -> {true, Digit2, Digit3};
try_simplify([Digit1, Digit], [Digit3, Digit]) -> {true, Digit1, Digit3};
try_simplify([Digit, Digit2], [Digit, Digit4]) -> {true, Digit2, Digit4};
try_simplify([Digit1, Digit], [Digit, Digit4]) -> {true, Digit1, Digit4};
try_simplify([_Digit1, _Digit2], [_Digit3, _Digit4]) -> false.

check_fraction(SourceNumerator, SourceDenominator, DestNumerator, DestDenominator) ->
    %% common denominator = SourceDenominator * DestDenominator
    (SourceNumerator * DestDenominator) == (DestNumerator * SourceDenominator).