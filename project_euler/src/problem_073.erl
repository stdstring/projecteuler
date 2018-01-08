%% @author std-string

%% Consider the fraction, n/d, where n and d are positive integers. If n < d and HCF(n,d)=1, it is called a reduced proper fraction.
%% If we list the set of reduced proper fractions for d <= 8 in ascending order of size, we get:
%% 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
%% It can be seen that there are 3 fractions between 1/3 and 1/2: 3/8, 2/5, 3/7.
%% How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d <= 12000?

-module(problem_073).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{{1, 3}, {1, 2}, 8}, 3}, {{{1, 3}, {1, 2}, 12000}, 7295372}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({Left, Right, MaxD}) -> process(Left, Right, MaxD).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(Left :: rational:rational_number(), Right :: rational:rational_number(), MaxD :: pos_integer()) -> non_neg_integer().
process(Left, Right, MaxD) -> process_denominator(2, Left, Right, MaxD, 0).

-spec calc_numerator_range(Left :: rational:rational_number(), Right :: rational:rational_number(), D :: pos_integer()) ->
    {MinN :: pos_integer(), MaxN :: pos_integer()}.
calc_numerator_range({LeftN, LeftD}, {RightN, RightD}, D) ->
    MinN = LeftN * D div LeftD + 1,
    case RightN * D rem RightD of
        0 -> {MinN, RightN * D div RightD - 1};
        _Other -> {MinN, RightN * D div RightD}
    end.

-spec process_denominator(D :: pos_integer(),
                          Left :: rational:rational_number(),
                          Right :: rational:rational_number(),
                          MaxD :: pos_integer(),
                          Count :: non_neg_integer()) -> non_neg_integer().
process_denominator(D, _Left, _Right, MaxD, Count) when D > MaxD -> Count;
process_denominator(D, Left, Right, MaxD, Count) ->
    {MinN, MaxN} = calc_numerator_range(Left, Right, D),
    if
        (MinN rem 2 == 0) and (D rem 2 == 0) -> process_denominator(D + 1, Left, Right, MaxD, process_numerator(MinN + 1, MaxN, 2, D, Count));
        (MinN rem 2 == 1) and (D rem 2 == 0) -> process_denominator(D + 1, Left, Right, MaxD, process_numerator(MinN, MaxN, 2, D, Count));
        (MinN rem 2 == 0) and (D rem 2 == 1) -> process_denominator(D + 1, Left, Right, MaxD, process_numerator(MinN, MaxN, 1, D, Count));
        (MinN rem 2 == 1) and (D rem 2 == 1) -> process_denominator(D + 1, Left, Right, MaxD, process_numerator(MinN, MaxN, 1, D, Count))
    end.

-spec process_numerator(N :: pos_integer(),
                        MaxN :: pos_integer(),
                        DeltaN :: pos_integer(),
                        D :: pos_integer(),
                        Count :: non_neg_integer()) -> non_neg_integer().
process_numerator(N, MaxN, _DeltaN, _D, Count) when N > MaxN -> Count;
process_numerator(N, MaxN, DeltaN, D, Count) ->
    case number_dividers:calc_gcd(N, D) of
        1 -> process_numerator(N + DeltaN, MaxN, DeltaN, D, Count + 1);
        _Other -> process_numerator(N + DeltaN, MaxN, DeltaN, D, Count)
    end.