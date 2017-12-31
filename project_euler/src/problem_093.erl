%% @author std-string

%% By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic operations (+, −, *, /) and brackets/parentheses,
%% it is possible to form different positive integer targets.
%% For example,
%% 8 = (4 * (1 + 3)) / 2
%% 14 = 4 * (3 + 1 / 2)
%% 19 = 4 * (2 + 3) − 1
%% 36 = 3 * 4 * (2 + 1)
%% Note that concatenations of the digits, like 12 + 34, are not allowed.
%% Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum,
%% and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.
%% Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 1 to n, can be obtained,
%% giving your answer as a string: abcd.

-module(problem_093).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type digits_processing() :: numbers:digits() | 'stop'.
-type operator() :: char().
-type operators() :: [operator()].
-type operators_processing() :: operators() | 'stop'.
-type value() :: rational:rational_number() | 'nan'.
-type values() :: [value()].
-type values_bunch() :: [values()].
-type values_storage() :: array:array(boolean()).
-type result() :: {Count :: non_neg_integer(), Digits :: number:digits()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, "1258"}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) ->
    {_Count, [Digit1, Digit2, Digit3, Digit4]} = process_digits([0, 1, 2, 3], {0, []}),
    [$0 + Digit1, $0 + Digit2, $0 + Digit3, $0 + Digit4].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_digits(Digits :: digits_processing(), ProcessResult :: result()) -> result().
process_digits(stop, ProcessResult) -> ProcessResult;
process_digits(Digits, ProcessResult) -> process_digits(get_next_digits_combination(Digits), process_operators(Digits, ProcessResult)).

-spec process_operators(Digits :: numbers:digits(), ProcessResult :: result()) -> result().
process_operators(Digits, ProcessResult) ->
    Alphabet = array:from_list(Digits),
    LexNumberSup = permutations:get_lexicographical_number_sup(Alphabet),
    DigitsCombinations = lists:map(fun(LexNumber) -> permutations:get_permutation(LexNumber, Alphabet) end, lists:seq(0, LexNumberSup - 1)),
    process_operators(Digits, DigitsCombinations, [$+, $+, $+], array:new([{default, false}]), ProcessResult).

-spec process_operators(Digits :: numbers:digits(),
                        DigitsCombinations :: [numbers:digits()],
                        Operators :: operators_processing(),
                        Storage :: values_storage(),
                        ProcessResult :: result()) -> result().
process_operators(Digits, _DigitsCombinations, stop, Storage, {SavedCount, _SavedDigits} = ProcessResult) ->
    Count = process_values(Storage),
    if
        SavedCount < Count -> {Count, Digits};
        SavedCount >= Count -> ProcessResult
    end;
process_operators(Digits, DigitsCombinations, Operators, Storage, ProcessResult) ->
    Values = lists:map(fun(DigitsCombination) -> calc_digits_values(DigitsCombination, Operators) end, DigitsCombinations),
    process_operators(Digits, DigitsCombinations, get_next_operator_combination(Operators), collect_values(Values, Storage), ProcessResult).

-spec process_values(Storage :: values_storage()) -> non_neg_integer().
process_values(Storage) -> process_values(0, array:size(Storage), Storage, 0).

-spec process_values(Index :: non_neg_integer(),
                     Size :: pos_integer(),
                     Storage :: values_storage(),
                     Count :: non_neg_integer()) -> non_neg_integer().
process_values(Size, Size, _Storage, Count) -> Count;
process_values(Index, Size, Storage, Count) ->
    case array:get(Index, Storage) of
        true -> process_values(Index + 1, Size, Storage, Count + 1);
        false -> Count
    end.

-spec calc_digits_values(Digits :: numbers:digits(), Operators :: operators()) -> values().
calc_digits_values([Digit1, Digit2, Digit3, Digit4], [Operator1, Operator2, Operator3]) ->
    [
    %% Digit1 Operator1 Digit2 Operator2 Digit3 Operator3 Digit4
    calc_value(Digit1, Operator1, Digit2, Operator2, Digit3, Operator3, Digit4),
    %% (Digit1 Operator1 Digit2) Operator2 Digit3 Operator3 Digit4
    calc_value(calc_value(Digit1, Operator1, Digit2), Operator2, Digit3, Operator3, Digit4),
    %% Digit1 Operator1 (Digit2 Operator2 Digit3) Operator3 Digit4
    calc_value(Digit1, Operator1, calc_value(Digit2, Operator2, Digit3), Operator3, Digit4),
    %% Digit1 Operator1 Digit2 Operator2 (Digit3 Operator3 Digit4)
    calc_value(Digit1, Operator1, Digit2, Operator2, calc_value(Digit3, Operator3, Digit4)),
    %% (Digit1 Operator1 Digit2) Operator2 (Digit3 Operator3 Digit4)
    calc_value(calc_value(Digit1, Operator1, Digit2), Operator2, calc_value(Digit3, Operator3, Digit4)),
    %% (Digit1 Operator1 Digit2 Operator2 Digit3) Operator3 Digit4
    calc_value(calc_value(Digit1, Operator1, Digit2, Operator2, Digit3), Operator3, Digit4),
    %% ((Digit1 Operator1 Digit2) Operator2 Digit3) Operator3 Digit4
    calc_value(calc_value(calc_value(Digit1, Operator1, Digit2), Operator2, Digit3), Operator3, Digit4),
    %% (Digit1 Operator1 (Digit2 Operator2 Digit3)) Operator3 Digit4
    calc_value(calc_value(Digit1, Operator1, calc_value(Digit2, Operator2, Digit3)), Operator3, Digit4),
    %% Digit1 Operator1 (Digit2 Operator2 Digit3 Operator3 Digit4)
    calc_value(Digit1, Operator1, calc_value(Digit2, Operator2, Digit3, Operator3, Digit4)),
    %% Digit1 Operator1 ((Digit2 Operator2 Digit3) Operator3 Digit4)
    calc_value(Digit1, Operator1, calc_value(calc_value(Digit2, Operator2, Digit3), Operator3, Digit4)),
    %% Digit1 Operator1 (Digit2 Operator2 (Digit3 Operator3 Digit4))
    calc_value(Digit1, Operator1, calc_value(Digit2, Operator2, calc_value(Digit3, Operator3, Digit4)))
    ].

-spec collect_values(ValuesBunch :: values_bunch(), Storage :: values_storage()) -> values_storage().
collect_values(Source, Storage) -> collect_values([], Source, Storage).

-spec collect_values(Values :: values(), ValuesBunch :: values_bunch(), Storage :: values_storage()) -> values_storage().
collect_values([], [], Storage) -> Storage;
collect_values([], [Source | SourceRest], Storage) -> collect_values(Source, SourceRest, Storage);
collect_values([{Numerator, Denominator} | ValuesRest], SourceRest, Storage) when Numerator rem Denominator /= 0 ->
    collect_values(ValuesRest, SourceRest, Storage);
collect_values([{Numerator, Denominator} | ValuesRest], SourceRest, Storage) when Numerator rem Denominator == 0 ->
    collect_values(ValuesRest, SourceRest, collect_value(Numerator div Denominator, Storage));
collect_values([Value | ValuesRest], SourceRest, Storage) ->
    collect_values(ValuesRest, SourceRest, collect_value(Value, Storage)).

-spec collect_value(Value :: integer(), Storage :: values_storage()) -> values_storage().
collect_value(nan, Storage) -> Storage;
collect_value(Value, Storage) when Value =< 0 -> Storage;
collect_value(Value, Storage) -> array:set(Value - 1, true, Storage).

-spec calc_value(Value1 :: value(),
                 Operator1 :: operator(),
                 Value2 :: value(),
                 Operator2 :: operator(),
                 Value3 :: value(),
                 Operator3 :: operator(),
                 Value4 :: value()) -> value().
calc_value(Value1, $*, Value2, Operator2, Value3, Operator3, Value4) -> calc_value(calc_value(Value1, $*, Value2), Operator2, Value3, Operator3, Value4);
calc_value(Value1, $/, Value2, Operator2, Value3, Operator3, Value4) -> calc_value(calc_value(Value1, $/, Value2), Operator2, Value3, Operator3, Value4);
calc_value(Value1, Operator1, Value2, $*, Value3, Operator3, Value4) -> calc_value(Value1, Operator1, calc_value(Value2, $*, Value3), Operator3, Value4);
calc_value(Value1, Operator1, Value2, $/, Value3, Operator3, Value4) -> calc_value(Value1, Operator1, calc_value(Value2, $/, Value3), Operator3, Value4);
calc_value(Value1, Operator1, Value2, Operator2, Value3, $*, Value4) -> calc_value(Value1, Operator1, Value2, Operator2, calc_value(Value3, $*, Value4));
calc_value(Value1, Operator1, Value2, Operator2, Value3, $/, Value4) -> calc_value(Value1, Operator1, Value2, Operator2, calc_value(Value3, $/, Value4));
calc_value(Value1, Operator1, Value2, Operator2, Value3, Operator3, Value4) ->
    calc_value(calc_value(Value1, Operator1, Value2), Operator2, Value3, Operator3, Value4).

-spec calc_value(Value1 :: value(), Operator1 :: operator(), Value2 :: value(), Operator2 :: operator(), Value3 :: value()) -> value().
calc_value(Value1, $*, Value2, Operator2, Value3) -> calc_value(calc_value(Value1, $*, Value2), Operator2, Value3);
calc_value(Value1, $/, Value2, Operator2, Value3) -> calc_value(calc_value(Value1, $/, Value2), Operator2, Value3);
calc_value(Value1, Operator1, Value2, $*, Value3) -> calc_value(Value1, Operator1, calc_value(Value2, $*, Value3));
calc_value(Value1, Operator1, Value2, $/, Value3) -> calc_value(Value1, Operator1, calc_value(Value2, $/, Value3));
calc_value(Value1, Operator1, Value2, Operator2, Value3) -> calc_value(calc_value(Value1, Operator1, Value2), Operator2, Value3).

-spec calc_value(Value1 :: value(), Operator :: operator(), Value2 :: value()) -> value().
calc_value(nan, _Operator, _Value2) -> nan;
calc_value(_Value1, _Operator, nan) -> nan;
calc_value(Value1, $+, Value2) -> rational:add(Value1, Value2);
calc_value(Value1, $-, Value2) -> rational:sub(Value1, Value2);
calc_value(Value1, $*, Value2) -> rational:mult(Value1, Value2);
calc_value(_Value1, $/, 0) -> nan;
calc_value(Value1, $/, Value2) -> rational:divide(Value1, Value2).

%% TODO (std_string) : think about generalization this functionality
-spec get_next_digits_combination(Digits :: numbers:digits()) -> digits_processing().
get_next_digits_combination([6]) -> stop;
get_next_digits_combination([Digit1]) -> [Digit1 + 1, Digit1 + 2, Digit1 + 3, Digit1 + 4];
get_next_digits_combination([Digit1, 7]) -> get_next_digits_combination([Digit1]);
get_next_digits_combination([Digit1, Digit2]) -> [Digit1, Digit2 + 1, Digit2 + 2, Digit2 + 3];
get_next_digits_combination([Digit1, Digit2, 8]) -> get_next_digits_combination([Digit1, Digit2]);
get_next_digits_combination([Digit1, Digit2, Digit3]) -> [Digit1, Digit2, Digit3 + 1, Digit3 + 2];
get_next_digits_combination([Digit1, Digit2, Digit3, 9]) -> get_next_digits_combination([Digit1, Digit2, Digit3]);
get_next_digits_combination([Digit1, Digit2, Digit3, Digit4]) ->
    case check_digits([Digit1, Digit2, Digit3, Digit4 + 1]) of
        true -> [Digit1, Digit2, Digit3, Digit4 + 1];
        false -> get_next_digits_combination([Digit1, Digit2, Digit3, Digit4 + 1])
    end.

-spec check_digits(Digits :: numbers:digits()) -> boolean().
check_digits([Digit, Digit, _Digit3, _Digit4]) -> false;
check_digits([Digit, _Digit2, Digit, _Digit4]) -> false;
check_digits([Digit, _Digit2, _Digit3, Digit]) -> false;
check_digits([_Digit1, Digit, Digit, _Digit4]) -> false;
check_digits([_Digit1, Digit, _Digit3, Digit]) -> false;
check_digits([_Digit1, _Digit2, Digit, Digit]) -> false;
check_digits([_Digit1, _Digit2, _Digit3, _Digit4]) -> true.

%% TODO (std_string) : think about generalization this functionality
%% order of operators: +, -, *, /
-spec get_next_operator_combination(Operators :: operators())-> operators_processing() | no_return().
get_next_operator_combination([$+])-> get_next_operator_combination([$-, $+, $+]);
get_next_operator_combination([$-])-> get_next_operator_combination([$*, $+, $+]);
get_next_operator_combination([$*])-> get_next_operator_combination([$/, $+, $+]);
get_next_operator_combination([$/])-> error(badarg);
get_next_operator_combination([Operator1, $+])-> get_next_operator_combination([Operator1, $-, $+]);
get_next_operator_combination([Operator1, $-])-> get_next_operator_combination([Operator1, $*, $+]);
get_next_operator_combination([Operator1, $*])-> get_next_operator_combination([Operator1, $/, $+]);
get_next_operator_combination([Operator1, $/])-> get_next_operator_combination([Operator1]);
get_next_operator_combination([$/, $/, $/]) -> stop;
get_next_operator_combination([Operator1, Operator2, $+]) -> [Operator1, Operator2, $-];
get_next_operator_combination([Operator1, Operator2, $-]) -> [Operator1, Operator2, $*];
get_next_operator_combination([Operator1, Operator2, $*]) -> [Operator1, Operator2, $/];
get_next_operator_combination([Operator1, Operator2, $/]) -> get_next_operator_combination([Operator1, Operator2]).