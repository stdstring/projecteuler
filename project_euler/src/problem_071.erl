%% @author std-string

%% Consider the fraction, n/d, where n and d are positive integers. If n < d and HCF(n,d) = 1, it is called a reduced proper fraction.
%% If we list the set of reduced proper fractions for d <= 8 in ascending order of size, we get:
%% 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
%% It can be seen that 2/5 is the fraction immediately to the left of 3/7.
%% By listing the set of reduced proper fractions for d <= 1000000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.

-module(problem_071).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% TODO (std_string) : move into common
-type compare_result() :: 'left' | 'equal' | 'right'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{8, {3, 7}}, 2}, {{1000000, {3, 7}}, 428570}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxDenominator, Fraction}) ->
    {Numerator, _Denominator} = process_denominator(2, {1, MaxDenominator}, MaxDenominator, Fraction),
    Numerator.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_denominator(Denominator :: pos_integer(),
                          SavedFraction :: rational:rational_fraction(),
                          MaxDenominator :: pos_integer(),
                          Fraction :: rational:rational_fraction()) -> rational:rational_fraction().
process_denominator(Denominator, SavedFraction, MaxDenominator, _Fraction) when Denominator > MaxDenominator -> SavedFraction;
process_denominator(Denominator, SavedFraction, MaxDenominator, Fraction) ->
    LeftNeighbour = find_left_neighbour(Denominator, Fraction),
    case compare_fractions(LeftNeighbour, Fraction) of
        equal -> process_denominator(Denominator + 1, SavedFraction, MaxDenominator, Fraction);
        right ->
            case compare_fractions(SavedFraction, LeftNeighbour) of
                equal -> process_denominator(Denominator + 1, SavedFraction, MaxDenominator, Fraction);
                left -> process_denominator(Denominator + 1, SavedFraction, MaxDenominator, Fraction);
                right -> process_denominator(Denominator + 1, LeftNeighbour, MaxDenominator, Fraction)
            end
    end.

-spec find_left_neighbour(Denominator :: pos_integer(), Fraction :: rational:rational_fraction()) -> rational:rational_fraction().
find_left_neighbour(Denominator, {FractionNumerator, FractionDenominator}) ->
    %% Numerator * FractionDenominator <= FractionNumerator * Denominator < (Numerator + 1) * FractionDenominator
    %% Numerator = FractionNumerator * Denominator div FractionDenominator
    {(FractionNumerator * Denominator) div FractionDenominator, Denominator}.

-spec compare_fractions(Left :: rational:rational_fraction(), Right :: rational:rational_fraction()) -> compare_result().
compare_fractions({LeftNumerator, LeftDenominator}, {RightNumerator, RightDenominator}) ->
    LeftResult = LeftNumerator * RightDenominator,
    RightResult = RightNumerator * LeftDenominator,
    if
        LeftResult < RightResult -> right;
        LeftResult > RightResult -> left;
        LeftResult == RightResult -> equal
    end.