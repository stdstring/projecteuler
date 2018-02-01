%% @author std-string

%% The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14.
%% As the sum of these divisors is equal to 28, we call it a perfect number.
%% Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers.
%% For this reason, 220 and 284 are called an amicable pair. Perhaps less well known are longer chains.
%% For example, starting with 12496, we form a chain of five numbers: 12496 → 14288 → 15472 → 14536 → 14264 → 12496 → ...
%% Since this chain returns to its starting point, it is called an amicable chain.
%% Find the smallest member of the longest amicable chain with no element exceeding one million.

-module(problem_095).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{1000000, 14316}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxNumber) ->
    DividersStorage = number_dividers:create_dividers(MaxNumber),
    Result = process(MaxNumber, DividersStorage),
    {MinIndex, _MaxSize} = array:foldl(fun(Index, Size, {SavedIndex, SavedSize}) ->
        if
            Size > SavedSize -> {Index, Size};
            true -> {SavedIndex, SavedSize}
        end
    end, {-1, 0}, Result),
    MinIndex + 2.

%% ====================================================================
%% Internal functions
%% ====================================================================


%% TODO (std_string) : think about name
process(MaxNumber, DividerStorage) ->
    process_number(2, MaxNumber, DividerStorage, array:new([{size, MaxNumber - 1}, {fixed, true}, {default, undef}])).

%% TODO (std_string) : think about name
process_number(Number, MaxNumber, _DividerStorage, ResultStorage) when Number > MaxNumber -> ResultStorage;
process_number(Number, MaxNumber, DividerStorage, ResultStorage) ->
    case array:get(Number - 2, ResultStorage) of
        undef -> process_number(Number + 1, MaxNumber, DividerStorage, process_chain(Number, MaxNumber, [], DividerStorage, ResultStorage));
        _Other -> process_number(Number + 1, MaxNumber, DividerStorage, ResultStorage)
    end.

%% TODO (std_string) : think about name
process_chain(Number, MaxNumber, Chain, _DividersStorage, ResultStorage) when Number > MaxNumber->
    set_chain_values(Chain, 0, 0, ResultStorage);
process_chain(1, _MaxNumber, Chain, _DividersStorage, ResultStorage) ->
    set_chain_values(Chain, 0, 0, ResultStorage);
process_chain(Number, MaxNumber, Chain, DividersStorage, ResultStorage) ->
    case array:get(Number - 2, ResultStorage) of
        undef ->
            case check_chain(Number, Chain, 0) of
                false -> process_chain(generate_next_number(Number, DividersStorage), MaxNumber, [Number] ++ Chain, DividersStorage, ResultStorage);
                {true, Size} ->
                    {CyclePart, NonCyclePart} = lists:split(Size + 1, Chain),
                    set_chain_values(NonCyclePart, 0, 0, set_chain_values(CyclePart, Size, 0, ResultStorage))
            end;
        _Size -> set_chain_values(Chain, 0, 0, ResultStorage)
    end.

%% TODO (std_string) : think about name
set_chain_values([], _Value, _Delta, ResultStorage) -> ResultStorage;
set_chain_values([Number | Rest], Value, Delta, ResultStorage) ->
    set_chain_values(Rest, Value + Delta, Delta, array:set(Number - 2, Value, ResultStorage)).

%% TODO (std_string) : think about name
generate_next_number(Number, DividersStorage) -> sets:fold(fun(Value, Dest) -> Value + Dest end, 0, array:get(Number - 2, DividersStorage)) - Number.
%%generate_next_number(Number, DividersStorage) -> lists:sum(sets:to_list(array:get(Number - 2, DividersStorage))) - Number.

%% TODO (std_string) : think about name
check_chain(_Number, [], _Size) -> false;
check_chain(Number, [Number | _Rest], Size) -> {true, Size};
check_chain(Number, [_OtherNumber | Rest], Size) -> check_chain(Number, Rest, Size + 1).