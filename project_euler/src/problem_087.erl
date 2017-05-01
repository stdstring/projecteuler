%% @author std-string

%% The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28.
%% In fact, there are exactly four numbers below fifty that can be expressed in such a way:
%% 28 = 2^2 + 2^3 + 2^4
%% 33 = 3^2 + 2^3 + 2^4
%% 49 = 5^2 + 2^3 + 2^4
%% 47 = 2^2 + 3^3 + 2^4
%% How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?

-module(problem_087).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type result() :: {Count :: non_neg_integer(), Storage :: mutable_uint8_array:mutable_uint8_array()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{50, 4}, {50000000, 1097343}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(NumberSup) ->
    ok = mutable_uint8_array:init(),
    PrimeSup = round(math:sqrt(NumberSup)),
    Primes = eratos_sieve:get_primes(PrimeSup),
    {Count, _Storage} = process_numbers(Primes, NumberSup),
    Count.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(Primes :: [pos_integer()], NumberSup :: pos_integer()) -> result().
process_numbers(Primes, NumberSup) -> process_first_member(Primes, Primes, NumberSup, {0, mutable_uint8_array:create(NumberSup, 0)}).

-spec process_first_member(Numbers1 :: [pos_integer()], Primes :: [pos_integer()], NumberSup :: pos_integer(), Result :: result()) -> result().
process_first_member([], _Primes, _NumberSup, Result) -> Result;
process_first_member([Number1 | NumberRest], Primes, NumberSup, Result) ->
    case check_number(Number1, 2, 2, NumberSup) of
        {true, _Value} -> process_first_member(NumberRest, Primes, NumberSup, process_second_member(Number1, Primes, Primes, NumberSup, Result));
        false -> Result
    end.

-spec process_second_member(Number1 :: pos_integer(),
                            Numbers2 :: [pos_integer()],
                            Primes :: [pos_integer()],
                            NumberSup :: pos_integer(),
                            Result :: result()) -> result().
process_second_member(_Number1, [], _Primes, _NumberSup, Result) -> Result;
process_second_member(Number1, [Number2 | NumberRest], Primes, NumberSup, Result) ->
    case check_number(Number1, Number2, 2, NumberSup) of
        {true, _Value} -> process_second_member(Number1, NumberRest, Primes, NumberSup, process_third_member(Number1, Number2, Primes, NumberSup, Result));
        false -> Result
    end.

-spec process_third_member(Number1 :: pos_integer(),
                           Number2 :: pos_integer(),
                           Numbers3 :: [pos_integer()],
                           NumberSup :: pos_integer(),
                           Result :: result()) -> result().
process_third_member(_Number1, _Number2, [], _NumberSup, Result) -> Result;
process_third_member(Number1, Number2, [Number3 | NumberRest], NumberSup, {Count, Storage} = Result) ->
    case check_number(Number1, Number2, Number3, NumberSup) of
        {true, Value} ->
            case mutable_uint8_array:get(Value - 1, Storage) of
                0 -> process_third_member(Number1, Number2, NumberRest, NumberSup, {Count + 1, mutable_uint8_array:set(Value - 1, 1, Storage)});
                1 -> process_third_member(Number1, Number2, NumberRest, NumberSup, Result)
            end;
        false -> Result
    end.

-spec check_number(Number1 :: pos_integer(),
                   Number2 :: pos_integer(),
                   Number3 :: pos_integer(),
                   NumberSup :: pos_integer()) -> {'true', Sum :: pos_integer()} | 'false'.
check_number(Number1, Number2, Number3, NumberSup) ->
    Sum = numbers:power(Number1, 2) + numbers:power(Number2, 3) + numbers:power(Number3, 4),
    case Sum < NumberSup of
        true -> {true, Sum};
        false -> false
    end.