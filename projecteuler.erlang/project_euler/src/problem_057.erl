%% @author std-string

%% It is possible to show that the square root of two can be expressed as an infinite continued fraction.
%% 2^(1/2) = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
%% By expanding this for the first four iterations, we get:
%% 1 + 1/2 = 3/2 = 1.5
%% 1 + 1/(2 + 1/2) = 7/5 = 1.4
%% 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
%% 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
%% The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985,
%% is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
%% In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

-module(problem_057).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{1000, 153}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(IterationCount) -> process_iteration(1, IterationCount, {3, 2}, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_iteration(Iteration :: pos_integer(),
                        IterationCount :: pos_integer(),
                        PrevIteration :: rational:rational_number(),
                        SuitableCount :: non_neg_integer()) -> pos_integer().
process_iteration(Iteration, IterationCount, _PrevIteration, SuitableCount) when Iteration > IterationCount -> SuitableCount;
process_iteration(Iteration, IterationCount, PrevIteration, SuitableCount) ->
    {Numerator, Denominator} = calc_next_iteration(PrevIteration),
    NumeratorLength = numbers:get_digits_count(Numerator),
    DenominatorLength = numbers:get_digits_count(Denominator),
    if
        NumeratorLength > DenominatorLength -> process_iteration(Iteration + 1, IterationCount, {Numerator, Denominator}, SuitableCount + 1);
        NumeratorLength =< DenominatorLength -> process_iteration(Iteration + 1, IterationCount, {Numerator, Denominator}, SuitableCount)
    end.

-spec calc_next_iteration(PrevIteration :: rational:rational_number()) -> rational:rational_number().
calc_next_iteration(PrevIteration) ->
    rational:simplify(rational:add(1, rational:reverse(rational:add(1, PrevIteration)))).