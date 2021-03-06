%% @author std-string

%% By replacing the 1-st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
%% By replacing the 3-rd and 4-th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers,
%% yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family,
%% is the smallest prime with this property.
%% Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

-module(problem_051).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MAX_NUMBER, 10000000-1).
-define(RANGE_START, 10).
-define(DIGITS_COUNT, 2).
-define(YOUNG_POSSIBLE_DIGITS, [1, 3, 7, 9]).

%% TODO (std_string) : probably, move into separate module
-type digits_pos() :: [DigitPos :: non_neg_integer()].
-type digits_info() :: array:array(digits_pos()).
-type process_result() :: {'true', Number :: pos_integer()} | 'false'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{6, 13}, {7, 56003}, {8, 121313}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(FamilySize) ->
    Primes = eratos_sieve:get_sieve(?MAX_NUMBER),
    case process_numbers(?RANGE_START, ?DIGITS_COUNT, FamilySize, Primes) of
        {true, Prime} -> Prime;
        false -> error(solution_not_found)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_digits_info(Number :: pos_integer(), DigitsCount :: pos_integer()) -> digits_info().
create_digits_info(Number, DigitsCount) ->
    Digits = numbers:get_digits(Number),
    InfoInit = array:new([{size, 10}, {fixed, true}, {default, []}]),
    AccInit = {InfoInit, DigitsCount - 1},
    {InfoFinal, _} = lists:foldl(fun(Digit, {Info, Pos}) -> {array:set(Digit, [Pos] ++ array:get(Digit, Info), Info), Pos - 1} end, AccInit, Digits),
    InfoFinal.

-spec select_possible_variants(DigitsInfo :: digits_info(), FamilySize :: pos_integer()) -> numbers:digits().
select_possible_variants(DigitsInfo, FamilySize) -> select_possible_variants(DigitsInfo, FamilySize, 0, []).

-spec select_possible_variants(DigitsInfo :: digits_info(),
                               FamilySize :: pos_integer(),
                               Digit :: numbers:digit(),
                               Storage :: numbers:digits()) -> numbers:digits().
select_possible_variants(_DigitsInfo, _FamilySize, Digit, Storage) when Digit > 9 -> Storage;
select_possible_variants(DigitsInfo, FamilySize, Digit, Storage) ->
    PosList = array:get(Digit, DigitsInfo),
    case check_possible_variant(PosList, Digit, FamilySize) of
        false -> select_possible_variants(DigitsInfo, FamilySize, Digit + 1, Storage);
        true -> select_possible_variants(DigitsInfo, FamilySize, Digit + 1, [Digit] ++ Storage)
    end.

-spec check_possible_variant(PosList :: digits_pos(), Digit :: numbers:digit(), FamilySize :: pos_integer()) -> boolean().
check_possible_variant([], _Digit, _FamilySize) -> false;
%% last (younger) digit must be 1, 3, 7, 9
check_possible_variant([0 | _Rest], 1, FamilySize) -> FamilySize =< 4;
check_possible_variant([0 | _Rest], 3, FamilySize) -> FamilySize =< 3;
check_possible_variant([0 | _Rest], 7, FamilySize) -> FamilySize =< 2;
check_possible_variant([0 | _Rest], 9, FamilySize) -> FamilySize =< 1;
check_possible_variant(_PosList, Digit, FamilySize) -> FamilySize =< (10 - Digit).

-spec calc_number_part(Digit :: numbers:digit(), PosList :: digits_pos()) -> non_neg_integer().
calc_number_part(0, _PosList) -> 0;
calc_number_part(_Digit, []) -> 0;
calc_number_part(Digit, PosList) -> Digit * lists:sum(lists:map(fun(Pos) -> numbers:power(10, Pos) end, PosList)).

-spec create_number(DigitsInfo :: digits_info()) -> non_neg_integer().
create_number(DigitsInfo) -> array:foldl(fun(Digit, PosList, Number) -> calc_number_part(Digit, PosList) + Number end, 0, DigitsInfo).

-spec check_family(DigitsInfo :: digits_info(),
                   Digit :: numbers:digit(),
                   FamilySize :: pos_integer(),
                   Primes :: eratos_sieve:sieve()) -> boolean().
check_family(DigitsInfo, Digit, FamilySize, Primes) ->
    PosList = array:get(Digit, DigitsInfo),
    InvDigitsInfo = array:set(Digit, [], DigitsInfo),
    InvNumberPart = create_number(InvDigitsInfo),
    case PosList of
        [0 | _Rest] ->
            Digits = lists:dropwhile(fun(Number) -> Number =< Digit end, ?YOUNG_POSSIBLE_DIGITS),
            check_family_impl(InvNumberPart, Digits, PosList, FamilySize - 1, Primes);
        _ ->
            Digits = lists:seq(Digit + 1, 9),
            check_family_impl(InvNumberPart, Digits, PosList, FamilySize - 1, Primes)
    end.

-spec check_family_impl(InvNumberPart :: non_neg_integer(),
                        Digits :: numbers:digits(),
                        PosList :: digits_pos(),
                        FamilySizeRest :: integer(),
                        Primes :: eratos_sieve:sieve()) -> boolean().
check_family_impl(_InvNumberPart, Digits, _PosList, FamilySizeRest, _Primes) when length(Digits) < FamilySizeRest -> false;
check_family_impl(_InvNumberPart, [], _PosList, 0, _Primes) -> true;
check_family_impl(InvNumberPart, [Digit | DigitsRest], PosList, FamilySizeRest, Primes) ->
    Number = InvNumberPart + calc_number_part(Digit, PosList),
    case eratos_sieve:is_prime(Number, Primes) of
        true -> check_family_impl(InvNumberPart, DigitsRest, PosList, FamilySizeRest - 1, Primes);
        false -> check_family_impl(InvNumberPart, DigitsRest, PosList, FamilySizeRest, Primes)
    end.

-spec check_possible_families(DigitsInfo :: digits_info(),
                              Variants :: numbers:digits(),
                              FamilySize :: pos_integer(),
                              Primes :: eratos_sieve:sieve()) -> boolean().
check_possible_families(_DigitsInfo, [], _FamilySize, _Primes) -> false;
check_possible_families(DigitsInfo, [Variant | VariantsRest], FamilySize, Primes) ->
    case check_family(DigitsInfo, Variant, FamilySize, Primes) of
        false -> check_possible_families(DigitsInfo, VariantsRest, FamilySize, Primes);
        true -> true
    end.

-spec process_range(Current :: pos_integer(),
                    Max :: pos_integer(),
                    DigitsCount :: pos_integer(),
                    FamilySize :: pos_integer(),
                    Primes :: eratos_sieve:sieve()) -> process_result().
process_range(Current, Max, _DigitsCount, _FamilySize, _Primes) when Current > Max -> false;
process_range(Current, Max, DigitsCount, FamilySize, Primes) ->
    case eratos_sieve:is_prime(Current, Primes) of
        true ->
            DigitsInfo = create_digits_info(Current, DigitsCount),
            PossibleVariants = select_possible_variants(DigitsInfo, FamilySize),
            case check_possible_families(DigitsInfo, PossibleVariants, FamilySize, Primes) of
                false -> process_range(Current + 2, Max, DigitsCount, FamilySize, Primes);
                true -> {true, Current}
            end;
        false -> process_range(Current + 2, Max, DigitsCount, FamilySize, Primes)
    end.

-spec process_numbers(RangeStart :: pos_integer(),
                      DigitsCount :: pos_integer(),
                      FamilySize :: pos_integer(),
                      Primes :: eratos_sieve:sieve()) -> process_result().
process_numbers(RangeStart, _DigitsCount, _FamilySize, _Primes) when RangeStart > ?MAX_NUMBER -> false;
process_numbers(RangeStart, DigitsCount, FamilySize, Primes) ->
    RangeFinish = 10 * RangeStart - 3,
    case process_range(RangeStart + 1, RangeFinish, DigitsCount, FamilySize, Primes) of
        false -> process_numbers(10 * RangeStart, DigitsCount + 1, FamilySize, Primes);
        {true, Prime} -> {true, Prime}
    end.