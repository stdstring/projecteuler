%% @author std-string

%% Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n.
%% For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
%% The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
%% Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
%% Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

-module(problem_070).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{10000000, 8319823}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    Storage = number_dividers:create_prime_dividers(MaxNumber),
    process(MaxNumber, Storage).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(MaxNumber :: pos_integer(), Storage :: number_dividers:dividers_storage()) -> pos_integer().
process(MaxNumber, Storage) -> process_impl(3, MaxNumber, Storage, 2, 2).

-spec process_impl(Number :: pos_integer(),
                   MaxNumber :: pos_integer(),
                   Storage :: number_dividers:dividers_storage(),
                   SavedNumber :: pos_integer(),
                   MinRatio :: float()) -> pos_integer().
process_impl(Number, MaxNumber, _Storage, SavedNumber, _MinRatio) when Number > MaxNumber -> SavedNumber;
process_impl(Number, MaxNumber, Storage, SavedNumber, MinRatio) ->
    Dividers = number_dividers:get_number_dividers(Number, Storage),
    CurrentValue = number_dividers:calc_euler_function(Number, sets:to_list(Dividers)),
    Ratio = Number / CurrentValue,
    if
        MinRatio =< Ratio -> process_impl(Number + 1, MaxNumber, Storage, SavedNumber, MinRatio);
        MinRatio > Ratio ->
            case is_permutation(Number, CurrentValue) of
                true -> process_impl(Number + 1, MaxNumber, Storage, Number, Ratio);
                false -> process_impl(Number + 1, MaxNumber, Storage, SavedNumber, MinRatio)
            end
    end.

%% permutations with repetitions
%% TODO (std_string) : think about moving into common libs
-spec is_permutation(Number1 :: pos_integer(), Number2 :: pos_integer()) -> boolean().
is_permutation(Number1, Number2) ->
    Number1Representation = calc_number_representation(Number1),
    Number2Representation = calc_number_representation(Number2),
    Number1Representation == Number2Representation.

%% TODO (std_string) : think about moving into common libs
-spec calc_number_representation(Number :: pos_integer()) -> tuple().
calc_number_representation(Number) -> calc_number_representation_impl(numbers:get_digits(Number), {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}).

-spec calc_number_representation_impl(Digits :: numbers:digits(), Result :: tuple()) -> tuple().
calc_number_representation_impl([], Result) -> Result;
calc_number_representation_impl([Digit | DigitsRest], Result) ->
    calc_number_representation_impl(DigitsRest, setelement(Digit + 1, Result, element(Digit + 1, Result) + 1)).