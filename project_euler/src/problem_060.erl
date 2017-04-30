%% @author std-string

%% The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime.
%% For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
%% Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

-module(problem_060).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(SIEVE_MAX, 100000000).
-define(SEARCH_MAX, 10000).

-type result_storage() :: [[PrimeNumber :: pos_integer()]].

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{4, 792}, {5, 26033}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(SetSize) ->
    Sieve = eratos_sieve:get_sieve(?SIEVE_MAX),
    InitStorage = init_storage(Sieve, 3, []),
    ResultStorage = control:for(SetSize - 1, InitStorage, fun(_Index, Storage) -> process_storage(Sieve, Storage, []) end),
    lists:min(lists:map(fun(Primes) -> lists:sum(Primes) end, ResultStorage)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec combine(Number1 :: pos_integer(), Number2 :: pos_integer(), Factor :: pos_integer()) -> pos_integer().
combine(Number1, Number2, Factor) when Factor < Number2 -> combine(Number1, Number2, 10 * Factor);
combine(Number1, Number2, Factor) -> Number1 * Factor + Number2.

-spec checkPrime(Primes :: [pos_integer()], CheckedPrime :: pos_integer(), Sieve :: eratos_sieve:sieve()) -> boolean().
checkPrime([], _CheckedPrime, _Sieve) -> true;
checkPrime([Prime | PrimesRest], CheckedPrime, Sieve) ->
    Combination1 = combine(Prime, CheckedPrime, 1),
    Combination2 = combine(CheckedPrime, Prime, 1),
    case eratos_sieve:is_prime(Combination1, Sieve) and eratos_sieve:is_prime(Combination2, Sieve) of
        true -> checkPrime(PrimesRest, CheckedPrime, Sieve);
        false -> false
    end.

-spec init_storage(Sieve :: eratos_sieve:sieve(), Prime :: pos_integer(), Storage :: result_storage()) -> result_storage().
init_storage(_Sieve, Prime, Storage) when Prime > ?SEARCH_MAX -> Storage;
init_storage(Sieve, 5, Storage) -> init_storage(Sieve, eratos_sieve:get_next_prime(5, Sieve), Storage);
init_storage(Sieve, Prime, Storage) -> init_storage(Sieve, eratos_sieve:get_next_prime(Prime, Sieve), [[Prime]] ++ Storage).

-spec process_storage(Sieve :: eratos_sieve:sieve(),
                      PrevStorage :: result_storage(),
                      NextStorage :: result_storage()) -> result_storage().
process_storage(_Sieve, [], NextStorage) -> NextStorage;
process_storage(Sieve, [[Prime | _] = Primes | PrevStorageRest], NextStorage) ->
    NextPrime = eratos_sieve:get_next_prime(Prime, Sieve),
    process_storage(Sieve, PrevStorageRest, process_primes(Sieve, Primes, NextPrime, NextStorage)).

-spec process_primes(Sieve :: eratos_sieve:sieve(),
                     Primes :: [pos_integer()],
                     CheckedPrime :: pos_integer(),
                     NextStorage :: result_storage()) -> result_storage().
process_primes(_Sieve, _Primes, CheckedPrime, NextStorage) when CheckedPrime > ?SEARCH_MAX -> NextStorage;
process_primes(Sieve, Primes, CheckedPrime, NextStorage) ->
    UpdatedNextStorage = case checkPrime(Primes, CheckedPrime, Sieve) of
        true -> [[CheckedPrime] ++ Primes] ++ NextStorage;
        false -> NextStorage
    end,
    NextCheckedPrime = eratos_sieve:get_next_prime(CheckedPrime, Sieve),
    process_primes(Sieve, Primes, NextCheckedPrime, UpdatedNextStorage).