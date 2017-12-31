%% @author std-string

%% It is well known that if the square root of a natural number is not an integer, then it is irrational.
%% The decimal expansion of such square roots is infinite without any repeating pattern at all.
%% The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.
%% For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.

-module(problem_080).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(P_START, 0).
-define(C_START, 0).
-define(NUMBER_START, 2).
-define(INTEGER_ROOT_START, 2).

-type number_data() :: {IntegerPart :: numbers:digits(), FractionPart :: numbers:digits()}.
-type digit_pair() :: {Digit :: numbers:digit(), Digit :: numbers:digit()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{2, 100}, 475}, {{100, 100}, 40886}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxNumber, DigitCount}) -> process_numbers(?NUMBER_START, MaxNumber, DigitCount, ?INTEGER_ROOT_START, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(Number :: pos_integer(),
                      MaxNumber :: pos_integer(),
                      DigitCount :: pos_integer(),
                      IntegerRoot :: pos_integer(),
                      Sum :: non_neg_integer()) -> non_neg_integer().
process_numbers(Number, MaxNumber, _DigitCount, _IntegerRoot, Sum) when Number > MaxNumber -> Sum;
process_numbers(Number, MaxNumber, DigitCount, IntegerRoot, Sum) when Number == (IntegerRoot * IntegerRoot) ->
    process_numbers(Number + 1, MaxNumber, DigitCount, IntegerRoot + 1, Sum);
process_numbers(Number, MaxNumber, DigitCount, IntegerRoot, Sum) ->
    {IntegerPart, FullFractionPart} = process_number(prepare_number(Number, DigitCount)),
    IntegerPartSize = length(IntegerPart),
    FractionPart = lists:sublist(FullFractionPart, DigitCount - IntegerPartSize),
    process_numbers(Number + 1, MaxNumber, DigitCount, IntegerRoot, Sum + lists:sum(IntegerPart) + lists:sum(FractionPart)).

-spec prepare_number(N :: pos_integer(), FractionSize :: pos_integer()) -> number_data().
prepare_number(N, FractionSize) ->
    InitegerDigits = split_digits(numbers:get_digits(N)),
    FractionDigits = control:for(FractionSize, [], fun(_Index, Storage) -> [{0, 0}] ++ Storage end),
    {InitegerDigits, FractionDigits}.

-spec split_digits(Digits :: numbers:digits()) -> [digit_pair()].
split_digits([D1]) -> [{0, D1}];
split_digits([D1, D2]) -> [{D1, D2}];
split_digits([D1, D2, D3]) -> [{0, D1}, {D2, D3}];
split_digits([D1, D2, D3, D4]) -> [{D1, D2}, {D3, D4}].

%% TODO (std_string) : probably, move this algorithm into separate module
%% Algorithm (from https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Digit-by-digit_calculation):
%% Write the original number in decimal form. The numbers are written similar to the long division algorithm, and, as in long division, the root will be written on the line above.
%% Now separate the digits into pairs, starting from the decimal point and going both left and right. The decimal point of the root will be above the decimal point of the square.
%% One digit of the root will appear above each pair of digits of the square.
%% Beginning with the left-most pair of digits, do the following procedure for each pair:
%% 1) Starting on the left, bring down the most significant (leftmost) pair of digits not yet used (if all the digits have been used, write "00") and
%% write them to the right of the remainder from the previous step (on the first step, there will be no remainder). In other words, multiply the remainder by 100 and add the two digits.
%% This will be the current value c.
%% 2) Find p, y and x, as follows:
%% 2a) Let p be the part of the root found so far, ignoring any decimal point. (For the first step, p = 0).
%% 2b) Determine the greatest digit x such that x(20p + x) <= c. We will use a new variable y = x(20p + x).
%%     Note: 20p + x is simply twice p, with the digit x appended to the right).
%%     Note: You can find x by guessing what c/(20·p) is and doing a trial calculation of y, then adjusting x upward or downward as necessary.
%% 2c) Place the digit x as the next digit of the root, i.e., above the two digits of the square you just brought down. Thus the next p will be the old p times 10 plus x.
%% 3) Subtract y from c to form a new remainder.
%% 4) If the remainder is zero and there are no more digits to bring down, then the algorithm has terminated. Otherwise go back to step 1 for another iteration.
-spec process_number(Source :: number_data()) -> number_data().
process_number({IntegerPart, FractionPart}) -> process_number({IntegerPart, FractionPart}, {[], []}, ?P_START, ?C_START).

-spec process_number(Source :: number_data(),
                     Result :: number_data(),
                     P :: non_neg_integer(),
                     C :: non_neg_integer()) -> number_data().
process_number({[], []}, {IntegerResult, FractionResult}, _P, _C) ->
    {lists:reverse(IntegerResult), lists:reverse(FractionResult)};
process_number({[], [{D1, D2} | FractionRest]}, {IntegerResult, FractionResult}, P, C) ->
    {Digit, NewP, NewC} = calc_next_digit(P, C, {D1, D2}),
    process_number({[], FractionRest}, {IntegerResult, [Digit] ++ FractionResult}, NewP, NewC);
process_number({[{D1, D2} | IntegerRest], FractionPart}, {IntegerResult, FractionResult}, P, C) ->
    {Digit, NewP, NewC} = calc_next_digit(P, C, {D1, D2}),
    process_number({IntegerRest, FractionPart}, {[Digit] ++ IntegerResult, FractionResult}, NewP, NewC).

-spec calc_next_digit(P :: non_neg_integer(), C :: non_neg_integer(), DigitPair :: digit_pair()) ->
    {Digit :: numbers:digit(), P :: non_neg_integer(), C  :: non_neg_integer()}.
calc_next_digit(P, C, {D1, D2}) ->
    Y = C * 100 + D1 * 10 + D2,
    Digit = find_digit(P, Y, 0),
    NewC = Y - Digit * (20 * P + Digit),
    NewP = P * 10 + Digit,
    {Digit, NewP, NewC}.

-spec find_digit(P :: non_neg_integer(), C :: non_neg_integer(), Digit :: numbers:digit()) -> numbers:digit().
find_digit(_P, _C, 9) -> 9;
find_digit(P, C, Digit) ->
    Value = Digit * (20 * P + Digit),
    NextValue = (Digit + 1) * (20 * P + (Digit + 1)),
    if
        Value == C -> Digit;
        (Value < C) and (NextValue > C) -> Digit;
        true -> find_digit(P, C, Digit + 1)
    end.