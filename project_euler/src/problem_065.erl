%% @author std-string

%% The square root of 2 can be written as an infinite continued fraction.
%% 2^(1/2) = 1 + 1 / (2 + 1 / (2 + 1 / (2 + 1 / (2 + ...))))
%% The infinite continued fraction can be written, 2^(1/2) = [1;(2)], (2) indicates that 2 repeats ad infinitum.
%% In a similar way, 23^(1/2) = [4;(1,3,1,8)].
%% It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations.
%% Let us consider the convergents for 2^(1/2).
%% 1 + 1/2 = 3/2
%% 1 + 1/(2 + 1/2) = 7/5
%% 1 + 1/(2 + 1/(2 + 1/2)) = 17/12
%% 1 + 1/(2 + 1/(2 + 1/(2 + 1/2)) = 41/29
%% Hence the sequence of the first ten convergents for 2^(1/2) are:
%% 1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
%% What is most surprising is that the important mathematical constant, e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
%% The first ten terms in the sequence of convergents for e are:
%% 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
%% The sum of digits in the numerator of the 10-th convergent is 1 + 4 + 5 + 7=17.
%% Find the sum of digits in the numerator of the 100-th convergent of the continued fraction for e.

-module(problem_065).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{10, 17}, {100, 272}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxTermNumber) ->
    Range = generate_e_fraction_range(MaxTermNumber),
    {N, _D} = process_range(Range, 0),
    lists:sum(numbers:get_digits(N)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_range(Range :: [pos_integer()], Result :: rational:rational_fraction() | 0) -> rational:rational_fraction().
process_range([], Result) -> rational:add(2, rational:reverse(Result));
process_range([Term | Rest], 0) -> process_range(Rest, Term);
process_range([Term | Rest], Result) ->
    NextResult = rational:add(Term, rational:reverse(Result)),
    process_range(Rest, NextResult).

-spec generate_e_fraction_range(Count :: pos_integer()) -> [pos_integer()].
generate_e_fraction_range(Count) ->
    FractionCount = Count - 1,
    TripleCount = FractionCount div 3,
    Result = lists:foldl(fun(K, Dest) -> [1, 2 * K, 1] ++ Dest end, [], lists:seq(1, TripleCount)),
    case FractionCount rem 3 of
        0 -> Result;
        1 -> [1] ++ Result;
        2 -> [2 * (TripleCount + 1), 1] ++ Result
    end.