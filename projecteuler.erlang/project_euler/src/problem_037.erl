%% @author std-string

%% The number 3797 has an interesting property.
%% Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7.
%% Similarly we can work from right to left: 3797, 379, 37, and 3.
%% Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
%% NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

-module(problem_037).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(EXPECTED_COUNT, 11).

-type number_data() :: {Digits :: numbers:digits(), DigitsCount :: pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 748317}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

%% for suitable number d0d1...dN :
%% d0 in [2, 3, 5, 7] due to source number is reduced to d0
%% dN in [3, 7] due to source number is prime and is reduced to dN
%% d1... in [1, 3, 7, 9] due to source number is reduced to prime number
-spec solve(PreparedInput :: term()) -> term().
solve(none) ->
    Result = process_numbers({[2, 3], 2}, [], 0),
    lists:sum(Result).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(NumberData :: number_data(), Found :: [pos_integer()], FoundCount :: non_neg_integer()) -> [pos_integer()].
process_numbers({NumberDigits, NumberDigitsCount}, Found, FoundCount) ->
    Number = numbers:get_number(NumberDigits),
    case check_number(Number, NumberDigitsCount) of
        false -> process_numbers(generate_next_number(NumberDigits, NumberDigitsCount), Found, FoundCount);
        true ->
            NewFound = [Number] ++ Found,
            NewFoundCount = FoundCount + 1,
            if
                NewFoundCount < ?EXPECTED_COUNT -> process_numbers(generate_next_number(NumberDigits, NumberDigitsCount), NewFound, NewFoundCount);
                NewFoundCount == ?EXPECTED_COUNT -> NewFound
            end
    end.

-spec generate_next_number(Digits :: numbers:digits(), DigitsCount :: pos_integer()) -> number_data().
generate_next_number(PrevNumberDigits, PrevNumberDigitsCount) ->
    case lists:reverse(PrevNumberDigits) of
        [3 | DigitsRest] -> generate_next_number(DigitsRest, PrevNumberDigitsCount, false, [7]);
        [7 | DigitsRest] -> generate_next_number(DigitsRest, PrevNumberDigitsCount, true, [3])
    end.

-spec generate_next_number(Digits :: numbers:digits(),
                           DigitsCount :: pos_integer(),
                           TransferAddition :: boolean(),
                           Result :: numbers:digits()) -> number_data().
generate_next_number([], DigitsCount, false, Result) -> {Result, DigitsCount};
generate_next_number([Digit | DigitsRest], DigitsCount, false, Result) -> generate_next_number(DigitsRest, DigitsCount, false, [Digit] ++ Result);
generate_next_number([2], DigitsCount, true, Result) -> {[3] ++ Result, DigitsCount};
generate_next_number([3], DigitsCount, true, Result) -> {[5] ++ Result, DigitsCount};
generate_next_number([5], DigitsCount, true, Result) -> {[7] ++ Result, DigitsCount};
generate_next_number([7], DigitsCount, true, Result) -> {[2, 1] ++ Result, DigitsCount + 1};
generate_next_number([1 | DigitsRest], DigitsCount, true, Result) -> generate_next_number(DigitsRest, DigitsCount, false, [3] ++ Result);
generate_next_number([3 | DigitsRest], DigitsCount, true, Result) -> generate_next_number(DigitsRest, DigitsCount, false, [7] ++ Result);
generate_next_number([7 | DigitsRest], DigitsCount, true, Result) -> generate_next_number(DigitsRest, DigitsCount, false, [9] ++ Result);
generate_next_number([9 | DigitsRest], DigitsCount, true, Result) -> generate_next_number(DigitsRest, DigitsCount, true, [1] ++ Result).

-spec check_number(Number :: pos_integer(), DigitsCount :: pos_integer()) -> boolean().
check_number(Number, DigitsCount) ->
    case check_right_reduce(Number) of
        false -> false;
        true ->
            Dividend = numbers:power(10, DigitsCount - 1),
            check_left_reduce(Number rem Dividend, Dividend)
    end.

-spec check_left_reduce(Number :: pos_integer(), Dividend :: pos_integer()) -> boolean().
check_left_reduce(_Number, 10) -> true;
check_left_reduce(Number, Dividend) ->
    case number_dividers:is_prime(Number) of
        false -> false;
        true ->
            NewDivident = Dividend div 10,
            check_left_reduce(Number rem NewDivident, NewDivident)
    end.

-spec check_right_reduce(Number :: non_neg_integer()) -> boolean().
check_right_reduce(0) -> true;
check_right_reduce(Number) ->
    case number_dividers:is_prime(Number) of
        false -> false;
        true -> check_right_reduce(Number div 10)
    end.