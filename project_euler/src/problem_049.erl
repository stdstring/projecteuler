%% @author std-string

%% The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
%% (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
%% There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
%% What 12-digit number do you form by concatenating the three terms in this sequence?

-module(problem_049).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MAX_NUMBER, 9999).
-define(MIN_NUMBER, 1000).
-define(SECOND_TERM_RANGE, [{1000, 5999}, {2000, 6499}, {3000, 6999}, {4000, 7499}, {5000, 7999}, {6000, 8499}, {7000, 8999}, {8000, 9499}, {9000, 9999}]).
-define(KNOWN_RESULT, {1487, 4817, 8147}).

%% TODO (std_string) : move into common
-type digit() :: 0..9.
-type digits() :: [digit()].
-type number_range() :: 1000..9999.
-type primes_seq() :: {FirstTerm :: number_range(), SecondTerm :: number_range(), ThirdTerm :: number_range()}.
-type primes_set() :: sets:set(Prime :: pos_integer()).
-type ranges_array() :: array:array({From :: pos_integer(), To :: pos_integer()}).

-record(task_state, {sieve :: primes_set(), second_term_ranges :: ranges_array()}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 296962999629}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

%% second term dy first term:
%% 1) for [1000, 1999] - [1000, 5999]
%% 2) for [2000, 2999] - [2000, 6499]
%% 3) for [3000, 3999] - [3000, 6999]
%% 4) for [4000, 4999] - [4000, 7499]
%% 5) for [5000, 5999] - [5000, 7999]
%% 6) for [6000, 6999] - [6000, 8499]
%% 7) for [7000, 7999] - [7000, 8999]
%% 8) for [8000, 8999] - [8000, 9499]
%% 9) for [9000, 9999] - [9000, 9999]
-spec solve(PreparedInput :: term()) -> term().
solve(none) ->
    Sieve = lists:dropwhile(fun(Number) -> Number < ?MIN_NUMBER end, eratos_sieve:get_primes(?MAX_NUMBER)),
    SieveSet = sets:from_list(Sieve),
    SecondTermRanges = array:from_list(?SECOND_TERM_RANGE),
    State = #task_state{sieve = SieveSet, second_term_ranges = SecondTermRanges},
    ProcessFun = fun(Prime, Result) -> process_prime(Prime, State) ++ Result end,
    ProcessResults = lists:foldl(ProcessFun, [], Sieve),
    FilterMapFun = fun({N1, N2, N3}) -> if {N1, N2, N3} == ?KNOWN_RESULT -> false; {N1, N2, N3} /= ?KNOWN_RESULT -> {true, N1 * 100000000 + N2 * 10000 + N3} end end,
    [Result] = lists:filtermap(FilterMapFun, ProcessResults),
    Result.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_prime(Prime :: number_range(), State :: #task_state{}) -> [primes_seq()].
process_prime(Prime, State) ->
    [D1, D2, D3, D4] = numbers:get_digits(Prime),
    SieveSet = State#task_state.sieve,
    SecondTermRanges = State#task_state.second_term_ranges,
    {SecondTermMin, SecondTermMax} = array:get(D1 - 1, SecondTermRanges),
    GenerateResult = generate_numbers(D1, D2, D3, D4),
    [{Prime, SecondTerm, ThirdTerm} || [D5, D6, D7, D8] <- GenerateResult,
                                       D5 /= 0,
                                       [D5, D6, D7, D8] /= [D1, D2, D3, D4],
                                       SecondTerm <- [D5 * 1000 + D6 * 100 + D7 * 10 + D8],
                                       (SecondTermMin =< SecondTerm) and (SecondTerm =< SecondTermMax),
                                       sets:is_element(SecondTerm, SieveSet),
                                       ThirdTerm <- [2 * SecondTerm - Prime],
                                       ThirdTerm < ?MAX_NUMBER,
                                       sets:is_element(ThirdTerm, SieveSet),
                                       check_digits([D1, D2, D3, D4], numbers:get_digits(ThirdTerm))].

-spec generate_numbers(D1 :: digit(), D2 :: digit(), D3 :: digit(), D4 :: digit()) -> [digits()].
generate_numbers(D1, D, D, D) ->
    [[D, D1, D, D], [D, D, D1, D], [D, D, D, D1]];
generate_numbers(D, D2, D, D) ->
    [[D2, D, D, D], [D, D, D2, D], [D, D, D, D2]];
generate_numbers(D, D, D3, D) ->
    [[D3, D, D, D], [D, D3, D, D], [D, D, D, D3]];
generate_numbers(D, D, D, D4) ->
    [[D4, D, D, D], [D, D4, D, D], [D, D, D4, D]];
generate_numbers(D1, D2, D, D) ->
    [[D1, D, D2, D],
     [D1, D, D, D2],
     [D2, D1, D, D],
     [D2, D, D1, D],
     [D2, D, D, D1],
     [D, D1, D2, D],
     [D, D1, D, D2],
     [D, D2, D1, D],
     [D, D2, D, D1],
     [D, D, D1, D2],
     [D, D, D2, D1]];
generate_numbers(D1, D, D3, D) ->
    [[D1, D3, D, D],
     [D1, D, D, D3],
     [D3, D1, D, D],
     [D3, D, D1, D],
     [D3, D, D, D1],
     [D, D1, D3, D],
     [D, D1, D, D3],
     [D, D3, D1, D],
     [D, D3, D, D1],
     [D, D, D1, D3],
     [D, D, D3, D1]];
generate_numbers(D1, D, D, D4) ->
    [[D1, D4, D, D],
     [D1, D, D4, D],
     [D4, D1, D, D],
     [D4, D, D1, D],
     [D4, D, D, D1],
     [D, D1, D4, D],
     [D, D1, D, D4],
     [D, D4, D1, D],
     [D, D4, D, D1],
     [D, D, D1, D4],
     [D, D, D4, D1]];
generate_numbers(D, D2, D3, D) ->
    [[D2, D3, D, D],
     [D2, D, D3, D],
     [D2, D, D, D3],
     [D3, D2, D, D],
     [D3, D, D2, D],
     [D3, D, D, D2],
     [D, D2, D, D3],
     [D, D3, D2, D],
     [D, D3, D, D2],
     [D, D, D2, D3],
     [D, D, D3, D2]];
generate_numbers(D, D2, D, D4) ->
    [[D2, D4, D, D],
     [D2, D, D4, D],
     [D4, D2, D, D],
     [D4, D, D2, D],
     [D4, D, D, D2],
     [D, D2, D4, D],
     [D, D2, D, D4],
     [D, D4, D2, D],
     [D, D4, D, D2],
     [D, D, D2, D4],
     [D, D, D4, D2]];
generate_numbers(D, D, D3, D4) ->
    [[D3, D4, D, D],
     [D3, D, D4, D],
     [D3, D, D, D4],
     [D4, D3, D, D],
     [D4, D, D3, D],
     [D4, D, D, D3],
     [D, D3, D4, D],
     [D, D3, D, D4],
     [D, D4, D3, D],
     [D, D4, D, D3],
     [D, D, D4, D3]];
generate_numbers(D1, D2, D3, D4) ->
    [[D1, D2, D4, D3],
     [D1, D3, D2, D4],
     [D1, D3, D4, D2],
     [D1, D4, D2, D3],
     [D1, D4, D3, D2],
     [D2, D1, D3, D4],
     [D2, D1, D4, D3],
     [D2, D3, D1, D4],
     [D2, D3, D4, D1],
     [D2, D4, D1, D3],
     [D2, D4, D3, D1],
     [D3, D1, D2, D4],
     [D3, D1, D4, D2],
     [D3, D2, D1, D4],
     [D3, D2, D4, D1],
     [D3, D4, D1, D2],
     [D3, D4, D2, D1],
     [D4, D1, D2, D3],
     [D4, D1, D3, D2],
     [D4, D2, D1, D3],
     [D4, D2, D3, D1],
     [D4, D3, D1, D2],
     [D4, D3, D2, D1]].

-spec check_digits(Expected :: digits(), Actual :: digits()) -> boolean().
check_digits([D1, D2, D3, D4], [D1, D2, D3, D4]) -> true;
check_digits([D1, D2, D3, D4], [D1, D2, D4, D3]) -> true;
check_digits([D1, D2, D3, D4], [D1, D3, D2, D4]) -> true;
check_digits([D1, D2, D3, D4], [D1, D3, D4, D2]) -> true;
check_digits([D1, D2, D3, D4], [D1, D4, D2, D3]) -> true;
check_digits([D1, D2, D3, D4], [D1, D4, D3, D2]) -> true;
check_digits([D1, D2, D3, D4], [D2, D1, D3, D4]) -> true;
check_digits([D1, D2, D3, D4], [D2, D1, D4, D3]) -> true;
check_digits([D1, D2, D3, D4], [D2, D3, D1, D4]) -> true;
check_digits([D1, D2, D3, D4], [D2, D3, D4, D1]) -> true;
check_digits([D1, D2, D3, D4], [D2, D4, D1, D3]) -> true;
check_digits([D1, D2, D3, D4], [D2, D4, D3, D1]) -> true;
check_digits([D1, D2, D3, D4], [D3, D1, D2, D4]) -> true;
check_digits([D1, D2, D3, D4], [D3, D1, D4, D2]) -> true;
check_digits([D1, D2, D3, D4], [D3, D2, D1, D4]) -> true;
check_digits([D1, D2, D3, D4], [D3, D2, D4, D1]) -> true;
check_digits([D1, D2, D3, D4], [D3, D4, D1, D2]) -> true;
check_digits([D1, D2, D3, D4], [D3, D4, D2, D1]) -> true;
check_digits([D1, D2, D3, D4], [D4, D1, D2, D3]) -> true;
check_digits([D1, D2, D3, D4], [D4, D1, D3, D2]) -> true;
check_digits([D1, D2, D3, D4], [D4, D2, D1, D3]) -> true;
check_digits([D1, D2, D3, D4], [D4, D2, D3, D1]) -> true;
check_digits([D1, D2, D3, D4], [D4, D3, D1, D2]) -> true;
check_digits([D1, D2, D3, D4], [D4, D3, D2, D1]) -> true;
check_digits([_D1, _D2, _D3, _D4], [_D5, _D6, _D7, _D8]) -> false.