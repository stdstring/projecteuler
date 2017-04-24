%% @author std-string

%% The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order,
%% but it also has a rather interesting sub-string divisibility property.
%% Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
%% d2d3d4=406 is divisible by 2
%% d3d4d5=063 is divisible by 3
%% d4d5d6=635 is divisible by 5
%% d5d6d7=357 is divisible by 7
%% d6d7d8=572 is divisible by 11
%% d7d8d9=728 is divisible by 13
%% d8d9d10=289 is divisible by 17
%% Find the sum of all 0 to 9 pandigital numbers with this property.

-module(problem_043).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% TODO (std_string) : move into common
-type digit() :: 0..9.
-type digits() :: [digit()].

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 16695334890}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) ->
    Digits = lists:seq(0, 9),
    Numbers = process_number(Digits, 17, []),
    lists:sum(Numbers).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% process d8d9d10
-spec process_number(Digits :: digit(), Number :: pos_integer(), Result :: [pos_integer()]) -> [pos_integer()].
process_number(_Digits, Number, Result) when Number > 999 -> Result;
process_number(Digits, Number, Result) ->
    [Digit8, Digit9, Digit10] = get_digits(Number),
    case check_d8d9d10_part(Digit8, Digit9, Digit10) of
        false -> process_number(Digits, Number + 17, Result);
        true ->
            DigitsRest =lists:filter(fun(Digit) -> (Digit /= Digit8) and (Digit /= Digit9) and (Digit /= Digit10) end, Digits),
            NewResult = process_number(DigitsRest, DigitsRest, Digit8, Digit9, Digit10, Result),
            process_number(Digits, Number + 17, NewResult)
    end.

%% process d7d8d9
-spec process_number(ProcessedDigits :: digits(),
                     Digits :: digits(),
                     Digit8 :: digit(),
                     Digit9 :: digit(),
                     Digit10 :: digit(),
                     Result :: [pos_integer()]) -> [pos_integer()].
process_number([], _Digits, _Digit8, _Digit9, _Digit10, Result) -> Result;
process_number([Digit7 | Digit7Rest], Digits, Digit8, Digit9, Digit10, Result) ->
    Number = numbers:get_number([Digit7, Digit8, Digit9]),
    case (Number rem 13) of
        0 ->
            DigitsRest = lists:filter(fun(Digit) -> Digit /= Digit7 end, Digits),
            NewResult = process_number(DigitsRest, DigitsRest, Digit7, Digit8, Digit9, Digit10, Result),
            process_number(Digit7Rest, Digits, Digit8, Digit9, Digit10, NewResult);
        _Other -> process_number(Digit7Rest, Digits, Digit8, Digit9, Digit10, Result)
    end.

%% process d6d7d8
-spec process_number(ProcessedDigits :: digits(),
                     Digits :: digits(),
                     Digit7 :: digit(),
                     Digit8 :: digit(),
                     Digit9 :: digit(),
                     Digit10 :: digit(),
                     Result :: [pos_integer()]) -> [pos_integer()].
process_number([], _Digits, _Digit7, _Digit8, _Digit9, _Digit10, Result) -> Result;
%% d6 = 0, 5 due to d4d5d6 is divisible by 5
process_number([Digit6 | Digit6Rest], Digits, Digit7, Digit8, Digit9, Digit10, Result) when Digit6 == 0; Digit6 == 5->
    Number = numbers:get_number([Digit6, Digit7, Digit8]),
    case (Number rem 11) of
        0 ->
            DigitsRest = lists:filter(fun(Digit) -> Digit /= Digit6 end, Digits),
            NewResult = process_number(DigitsRest, DigitsRest, Digit6, Digit7, Digit8, Digit9, Digit10, Result),
            process_number(Digit6Rest, Digits, Digit7, Digit8, Digit9, Digit10, NewResult);
        _Other -> process_number(Digit6Rest, Digits, Digit7, Digit8, Digit9, Digit10, Result)
    end;
process_number([_Digit6 | Digit6Rest], Digits, Digit7, Digit8, Digit9, Digit10, Result) ->
    process_number(Digit6Rest, Digits, Digit7, Digit8, Digit9, Digit10, Result).

%% process d5d6d7
-spec process_number(ProcessedDigits :: digits(),
                     Digits :: digits(),
                     Digit6 :: digit(),
                     Digit7 :: digit(),
                     Digit8 :: digit(),
                     Digit9 :: digit(),
                     Digit10 :: digit(),
                     Result :: [pos_integer()]) -> [pos_integer()].
process_number([], _Digits, _Digit6, _Digit7, _Digit8, _Digit9, _Digit10, Result) -> Result;
process_number([Digit5 | Digit5Rest], Digits, Digit6, Digit7, Digit8, Digit9, Digit10, Result) ->
    Number = numbers:get_number([Digit5, Digit6, Digit7]),
    case (Number rem 7) of
        0 ->
            DigitsRest = lists:filter(fun(Digit) -> Digit /= Digit5 end, Digits),
            NewResult = process_number(DigitsRest, DigitsRest, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result),
            process_number(Digit5Rest, Digits, Digit6, Digit7, Digit8, Digit9, Digit10, NewResult);
        _Other -> process_number(Digit5Rest, Digits, Digit6, Digit7, Digit8, Digit9, Digit10, Result)
    end.

%% process d4d5d6
-spec process_number(ProcessedDigits :: digits(),
                     Digits :: digits(),
                     Digit5 :: digit(),
                     Digit6 :: digit(),
                     Digit7 :: digit(),
                     Digit8 :: digit(),
                     Digit9 :: digit(),
                     Digit10 :: digit(),
                     Result :: [pos_integer()]) -> [pos_integer()].
process_number([], _Digits, _Digit5, _Digit6, _Digit7, _Digit8, _Digit9, _Digit10, Result) -> Result;
%% d4 = 0, 2, 4, 6, 8 due to d2d3d4 is divisible by 2
process_number([Digit4 | Digit4Rest], Digits, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result) when Digit4 == 0; Digit4 == 2; Digit4 ==4; Digit4 == 6; Digit4 == 8 ->
    %% Digit 6 == 0 or Difit6 == 5 - check in generate_d6d7d8_part
    DigitsRest = lists:filter(fun(Digit) -> Digit /= Digit4 end, Digits),
    NewResult = process_number(DigitsRest, DigitsRest, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result),
    process_number(Digit4Rest, Digits, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, NewResult);
process_number([_Digit4 | Digit4Rest], Digits, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result) ->
    process_number(Digit4Rest, Digits, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result).

%% process d3d4d5
-spec process_number(ProcessedDigits :: digits(),
                     Digits :: digits(),
                     Digit4 :: digit(),
                     Digit5 :: digit(),
                     Digit6 :: digit(),
                     Digit7 :: digit(),
                     Digit8 :: digit(),
                     Digit9 :: digit(),
                     Digit10 :: digit(),
                     Result :: [pos_integer()]) -> [pos_integer()].
process_number([], _Digits, _Digit4, _Digit5, _Digit6, _Digit7, _Digit8, _Digit9, _Digit10, Result) -> Result;
process_number([Digit3 | Digit3Rest], Digits, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result) ->
    Number =numbers:get_number([Digit3, Digit4, Digit5]),
    case (Number rem 3) of
        0 ->
            DigitsRest = lists:filter(fun(Digit) -> Digit /= Digit3 end, Digits),
            NewResult = collect(DigitsRest, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result),
            process_number(Digit3Rest, Digits, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, NewResult);
        _Other -> process_number(Digit3Rest, Digits, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result)
    end.

-spec collect(Digits :: digits(),
              Digit3 :: digit(),
              Digit4 :: digit(),
              Digit5 :: digit(),
              Digit6 :: digit(),
              Digit7 :: digit(),
              Digit8 :: digit(),
              Digit9 :: digit(),
              Digit10 :: digit(),
              Result :: [pos_integer()]) -> [pos_integer()].
collect([Digit1, 0], Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result) ->
    [numbers:get_number([Digit1, 0, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10])] ++ Result;
collect([0, Digit2], Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result) ->
    [numbers:get_number([Digit2, 0, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10])] ++ Result;
collect([Digit1, Digit2], Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10, Result) ->
    Number1 = numbers:get_number([Digit1, Digit2, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10]),
    Number2 = numbers:get_number([Digit2, Digit1, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, Digit10]),
    [Number1, Number2] ++ Result.

-spec check_d8d9d10_part(Digit8 :: digit(), Digit9 :: digit(), Digit10 :: digit()) -> boolean().
check_d8d9d10_part(Digit, Digit, _Digit10) -> false;
check_d8d9d10_part(Digit, _Digit9, Digit) -> false;
check_d8d9d10_part(_Digit8, Digit, Digit) -> false;
check_d8d9d10_part(_Digit8, _Digit9, _Digit10) -> true.

-spec get_digits(Number :: pos_integer()) -> digits().
get_digits(Number) when Number > 999 -> throw(badarg);
get_digits(Number) when Number < 100 -> [0] ++ numbers:get_digits(Number);
get_digits(Number) -> numbers:get_digits(Number).