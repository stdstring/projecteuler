%% @author std-string

%% It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
%% 9 = 7 + 2 * 1^2
%% 15 = 7 + 2 * 2^2
%% 21 = 3 + 2 * 3^2
%% 25 = 7 + 2 * 3^2
%% 27 = 19 + 2 * 2^2
%% 33 = 31 + 2 * 1^2
%% It turns out that the conjecture was false.
%% What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

-module(problem_046).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

 %% TODO (std_string) : think about this top border
-define(MAX_NUMBER, 10000).

-type storage_type() :: array:array('prime' | 'processed' | 'undef').

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() -> [{none, 5777}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) ->
    [2 | OddSieve] = eratos_sieve: get_primes(?MAX_NUMBER),
    %% 1 - first odd number
    OddCount = (?MAX_NUMBER div 2) - 1,
    InitStorage = array:new([{size, OddCount}, {fixed, true}, {default, undef}]),
    OddStorage = lists:foldl(fun(Number, Storage) -> array:set((Number - 3) div 2, prime, Storage) end, InitStorage, OddSieve),
    SquareList = generate_square_list(1, []),
    process_number(3, OddStorage, SquareList).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_square_list(Number :: pos_integer(), SquareList :: [pos_integer()]) -> [pos_integer()].
generate_square_list(Number, SquareList) when (Number * Number) > ?MAX_NUMBER -> lists:reverse(SquareList);
generate_square_list(Number, SquareList) ->
    generate_square_list(Number + 1, [Number * Number] ++ SquareList).

-spec process_number(Number :: pos_integer(), OddStorage :: storage_type(), SquareList :: [pos_integer()]) -> pos_integer() | no_return().
process_number(Number, _OddStorage, _SquareList) when Number > ?MAX_NUMBER -> error(logic_error);
process_number(Number, OddStorage, SquareList) ->
    Index = (Number - 3) div 2,
    case array:get(Index, OddStorage) of
        undef -> Number;
        processed -> process_number(Number + 2, OddStorage, SquareList);
        prime -> process_number(Number + 2, process_prime(Number, OddStorage, SquareList), SquareList)
    end.

-spec process_prime(Prime :: pos_integer(), OddStorage :: storage_type(), SquareList :: [pos_integer()]) -> storage_type().
process_prime(Prime, OddStorage, [SquareNumber | _SquareListRest]) when (Prime + 2 * SquareNumber) > ?MAX_NUMBER -> OddStorage;
process_prime(Prime, OddStorage, [SquareNumber | SquareListRest]) ->
    Number = Prime + 2 * SquareNumber,
    Index = (Number - 3) div 2,
    case array:get(Index, OddStorage) of
        prime -> process_prime(Prime, OddStorage, SquareListRest);
        _Other -> process_prime(Prime, array:set(Index, processed, OddStorage), SquareListRest)
    end.