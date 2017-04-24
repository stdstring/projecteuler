%% @author std-string

%% The first two consecutive numbers to have two distinct prime factors are:
%% 14 = 2 * 7
%% 15 = 3 * 5
%% The first three consecutive numbers to have three distinct prime factors are:
%% 644 = 2^2 * 7 * 23
%% 645 = 3 * 5 * 43
%% 646 = 2 * 17 * 19.
%% Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?

-module(problem_047).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-include("primes_def.hrl").

-type storage() :: array:array('prime' | sets:set(pos_integer()) | 'undef').
-type prime_dividers() :: sets:set(PrimeDivider :: pos_integer()).
-type check_result() :: {Result :: boolean(), Number :: pos_integer(), Storage :: storage()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{2, 14}, {3, 644}, {4, 134043}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(Count) ->
    FirstPrimes = first(?KNOWN_PRIMES, Count),
    MinNumber = lists:foldl(fun(Number, Product) -> Number * Product end, 1, FirstPrimes),
    InitStorage = array:new([{size, ?KNOWN_PRIME_TOP_BOUND - 1}, {fixed, false}, {default, undef}]),
    PrimeStorage = lists:foldl(fun(Number, Storage) -> array:set(Number - 2, prime, Storage) end, InitStorage, ?KNOWN_PRIMES),
    PreparedStorage = prepare_dividers(2, MinNumber, PrimeStorage),
    process_number(MinNumber, Count, PreparedStorage).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_number(Number :: pos_integer(), Count :: pos_integer(), Storage :: storage()) -> pos_integer().
process_number(Number, Count, Storage) ->
    case array:get(Number - 2, Storage) of
        prime -> process_number(Number + 1, Count, Storage);
        undef ->
            case check_number(Number, 0, Count, Storage) of
                {true, Number, _NewStorage} -> Number;
                {false, LastNumber, NewStorage} -> process_number(LastNumber + 1, Count, NewStorage)
            end
    end.

%% TODO (std_string) : move to common libs
-spec first(SourceList :: list(), Count :: pos_integer()) -> list().
first(SourceList, Count) -> first(SourceList, 0, Count, []).

-spec first(SourceList :: list(), Index :: non_neg_integer(), Count :: pos_integer(), Dest :: list()) -> list().
first(_SourceList, Count, Count, Dest) -> lists:reverse(Dest);
first([], _Index, _Count, _Dest) -> error(logic_error);
first([SourceHead | SourceListRest], Index, Count, Dest) -> first(SourceListRest, Index + 1, Count, [SourceHead] ++ Dest).

-spec get_prime_dividers(Number :: pos_integer(), Storage :: storage()) -> prime_dividers().
get_prime_dividers(Number, Storage) -> get_prime_dividers(Number, 2, Storage).

-spec get_prime_dividers(Number :: pos_integer(), Divider :: pos_integer(), Storage :: storage()) -> prime_dividers().
get_prime_dividers(Number, Divider, _Storage) when Divider * Divider > Number -> prime;
get_prime_dividers(Number, 2, Storage) ->
    case (Number rem 2) of
        0 -> sets:add_element(2, get_dividers(Number div 2, Storage));
        _Other -> get_prime_dividers(Number, 3, Storage)
    end;
get_prime_dividers(Number, Divider, Storage) ->
    case array:get(Divider - 2, Storage) of
        prime ->
            case (Number rem Divider) of
                0 -> sets:add_element(Divider, get_dividers(Number div Divider, Storage));
                _Other -> get_prime_dividers(Number, Divider + 2, Storage)
            end;
        _Other -> get_prime_dividers(Number, Divider + 2, Storage)
    end.

-spec get_dividers(Number :: pos_integer(), Storage :: storage()) -> prime_dividers().
get_dividers(Number, Storage) ->
    case array:get(Number - 2, Storage) of
        prime -> sets:add_element(Number, sets:new());
        Dividers -> Dividers
    end.

-spec prepare_dividers(Number :: pos_integer(), MinNumber :: pos_integer(), Storage :: storage()) -> storage().
prepare_dividers(MinNumber, MinNumber, Storage) -> Storage;
prepare_dividers(Number, MinNumber, Storage) ->
    case array:get(Number - 2, Storage) of
        undef ->
            Dividers = get_prime_dividers(Number, Storage),
            prepare_dividers(Number, MinNumber, array:set(Number - 2, Dividers, Storage));
        _Other -> prepare_dividers(Number + 1, MinNumber, Storage)
    end.

-spec check_number(StartNumber :: pos_integer(), Index :: non_neg_integer(), Count :: pos_integer(), Storage :: storage()) -> check_result().
check_number(StartNumber, Count, Count, Storage) -> {true, StartNumber, Storage};
check_number(StartNumber, Index, Count, Storage) ->
    Number = StartNumber + Index,
    case array:get(Number - 2, Storage) of
        prime -> {false, Number, Storage};
        undef ->
            Dividers = get_prime_dividers(Number, Storage),
            NewStorage = array:set(Number - 2, Dividers, Storage),
            case check_dividers(Dividers, Count) of
                false -> {false, Number, NewStorage};
                true -> check_number(StartNumber, Index + 1, Count, NewStorage)
            end
    end.

-spec check_dividers(Dividers :: 'prime' | prime_dividers(), Count :: pos_integer()) -> boolean().
check_dividers(prime, _Count) -> false;
check_dividers(Dividers, Count) -> sets:size(Dividers) == Count.