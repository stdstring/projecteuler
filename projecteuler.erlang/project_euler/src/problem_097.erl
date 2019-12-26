%% @author std-string

%% The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne prime of the form 2^6972593-1;
%% it contains exactly 2,098,960 digits.
%% Subsequently other Mersenne primes, of the form 2^p-1, have been found which contain more digits.
%% However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433*2^7830457+1.
%% Find the last ten digits of this prime number.

-module(problem_097).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{10, "8739992577"}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(LastDigitsCount) ->
    TopBound = numbers:power(10, LastDigitsCount),
    PowerValue = control:for(1, 7830457, 1, fun(_, Acc) -> (2 * Acc) rem TopBound end),
    PrimeValue = (28433 * PowerValue + 1) rem TopBound,
    PrimeValueStr = integer_to_list(PrimeValue),
    lists:duplicate(LastDigitsCount - length(PrimeValueStr), $0) ++ PrimeValueStr.

%% ====================================================================
%% Internal functions
%% ====================================================================