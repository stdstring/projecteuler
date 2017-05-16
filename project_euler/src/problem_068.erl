%% @author std-string

%% Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine (see https://projecteuler.net/problem=68).
%% Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely.
%% For example, the one of the solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.
%% It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
%% Total   Solution Set
%% 9       4,2,3; 5,3,1; 6,1,2
%% 9       4,3,2; 6,2,1; 5,1,3
%% 10      2,3,5; 4,5,1; 6,1,3
%% 10      2,5,3; 6,3,1; 4,1,5
%% 11      1,4,6; 3,6,2; 5,2,4
%% 11      1,6,4; 5,4,2; 3,2,6
%% 12      1,5,6; 2,6,4; 3,4,5
%% 12      1,6,5; 3,5,4; 2,4,6
%% By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.
%% Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings.
%% What is the maximum 16-digit string for a "magic" 5-gon ring (see https://projecteuler.net/problem=68)?

-module(problem_068).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type number_item() :: pos_integer().
-type numbers() :: [number_item()].
-type numbers_combination() :: numbers() | 'stop'.
-type numbers_chain() :: [{Number1 :: number_item(), Number2 :: number_item(), Number3 :: number_item()}].
-type numbers_chain_data() :: {MinNumber :: number_item(), Chain :: numbers_chain()}.
-type numbers_string() :: string().
-type next_numbers_provider() :: fun((Digits :: numbers()) -> numbers_combination()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{{6, 9, fun get_next_numbers3_combination/1}, "432621513"}, {{10, 16, fun get_next_numbers5_combination/1}, "6531031914842725"}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxNumber, Length, NextNumbersProvider}) ->
    InitNumbers = lists:seq(1, MaxNumber div 2),
    process(InitNumbers, MaxNumber, Length, NextNumbersProvider, "").

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(Numbers :: numbers_combination(),
              MaxNumber :: number_item(),
              Length :: pos_integer(),
              NextNumbersProvider :: next_numbers_provider(),
              SavedChain :: numbers_string()) -> numbers_string().
process(stop, _MaxNumber, _Length, _NextNumbersProvider, SavedChain) -> SavedChain;
process(Numbers, MaxNumber, Length, NextNumbersProvider, SavedChain) ->
    ChainsData = generate_chains(Numbers, MaxNumber),
    case lists:filter(fun(Chain) -> length(Chain) =:= Length end, lists:map(fun get_proper_chain_form/1, ChainsData)) of
        [] -> process(NextNumbersProvider(Numbers), MaxNumber, Length, NextNumbersProvider, SavedChain);
        Chains ->
            MaxChain = lists:max(Chains),
            if
                MaxChain > SavedChain -> process(NextNumbersProvider(Numbers), MaxNumber, Length, NextNumbersProvider, MaxChain);
                MaxChain =< SavedChain -> process(NextNumbersProvider(Numbers), MaxNumber, Length, NextNumbersProvider, SavedChain)
            end
    end.

-spec get_proper_chain_form(ChainData :: numbers_chain_data()) -> numbers_string().
get_proper_chain_form({MinNumber, Chain}) -> get_proper_chain_form(MinNumber, Chain).

-spec get_proper_chain_form(MinNumber :: number_item(), Chain :: numbers_chain()) -> numbers_string().
get_proper_chain_form(MinNumber, [{MinNumber, _Number2, _Number3} | _] = Chain) ->
    lists:flatten(lists:map(fun({N1, N2, N3}) -> integer_to_list(N1) ++ integer_to_list(N2) ++ integer_to_list(N3) end, Chain));
get_proper_chain_form(MinNumber, [{_Number1, _Number2, _Number3} | _] = Chain) ->
    get_proper_chain_form(MinNumber, collections:get_circular_shift(Chain)).

-spec generate_chains(SourceNumbers :: numbers(), MaxNumber :: number_item()) -> [numbers_chain_data()].
generate_chains([N1 | NumbersRest] = SourceNumbers, MaxNumber) ->
    [N2 | _] = NumbersRest,
    [MinNumber | _] = FreeNumbers = lists:subtract(lists:seq(1, MaxNumber), SourceNumbers),
    FreeNumbersCombinations = collections:get_all_circular_shift(FreeNumbers),
    DestNumbers = NumbersRest ++ [N1],
    MapFunc = fun([N3 | FreeNumbersRest]) -> generate_chain(DestNumbers, FreeNumbersRest, N1 + N2 + N3, [{N3, N1, N2}]) end,
    FilterFun = fun({true, _Chain}) -> true;
                   (false) -> false end,
    lists:map(fun({true, Chain}) -> {MinNumber, Chain} end, lists:filter(FilterFun, lists:map(MapFunc, FreeNumbersCombinations))).

-spec generate_chain(Numbers :: numbers(),
                     FreeNumbers :: numbers(),
                     ChainSum :: pos_integer(),
                     Chain :: numbers_chain()) -> {'true', Chain :: numbers_chain()} | 'false'.
generate_chain(_Numbers, [], _ChainSum, Chain) -> {true, lists:reverse(Chain)};
generate_chain([N1 | NumbersRest], FreeNumbers, ChainSum, Chain) ->
    [N2 | _] = NumbersRest,
    N3 = ChainSum - N1 - N2,
    case lists:subtract(FreeNumbers, [N3]) of
        FreeNumbers -> false;
        FreeNumbersRest -> generate_chain(NumbersRest, FreeNumbersRest, ChainSum, [{N3, N1, N2}] ++ Chain)
    end.

%% range for Number = 1 .. 6
%% TODO (std_string) : think about generalization this functionality
-spec get_next_numbers3_combination(Numbers :: numbers()) -> numbers_combination().
get_next_numbers3_combination([6]) -> stop;
get_next_numbers3_combination([N1]) ->
    %% next combination = N1 + 1, 1, 2
    get_next_numbers3_combination([N1 + 1, 1, 1]);
get_next_numbers3_combination([N1, 6]) -> get_next_numbers3_combination([N1]);
get_next_numbers3_combination([N1, N2]) ->
     %% next combination = N1, N2 + 1, 1
    get_next_numbers3_combination([N1, N2 + 1, 0]);
get_next_numbers3_combination([N1, N2, 6]) -> get_next_numbers3_combination([N1, N2]);
get_next_numbers3_combination([N1, N2, N3]) ->
    case check_numbers3([N1, N2, N3 + 1]) of
        true -> [N1, N2, N3 + 1];
        false -> get_next_numbers3_combination([N1, N2, N3 + 1])
    end.

%% range for Number = 1 .. 10
%% TODO (std_string) : think about generalization this functionality
-spec get_next_numbers5_combination(Numbers :: numbers()) -> numbers_combination().
get_next_numbers5_combination([10]) -> stop;
get_next_numbers5_combination([N1]) ->
    %% next combination = N1 + 1, 1, 2, 3, 4
    get_next_numbers5_combination([N1 + 1, 1, 2, 3, 3]);
get_next_numbers5_combination([N1, 10]) -> get_next_numbers5_combination([N1]);
get_next_numbers5_combination([N1, N2]) ->
    %% next combination = N1, N2 + 1, 1, 2, 3
    get_next_numbers5_combination([N1, N2 + 1, 1, 2, 2]);
get_next_numbers5_combination([N1, N2, 10]) -> get_next_numbers5_combination([N1, N2]);
get_next_numbers5_combination([N1, N2, N3]) ->
    %% next combination = N1, N2, N3 + 1, 1, 2
    get_next_numbers5_combination([N1, N2, N3 + 1, 1, 1]);
get_next_numbers5_combination([N1, N2, N3, 10]) -> get_next_numbers5_combination([N1, N2, N3]);
get_next_numbers5_combination([N1, N2, N3, N4]) ->
    %% next combination = N1, N2, N3, N4 + 1, 1
    get_next_numbers5_combination([N1, N2, N3, N4 + 1, 0]);
get_next_numbers5_combination([N1, N2, N3, N4, 10]) -> get_next_numbers5_combination([N1, N2, N3, N4]);
get_next_numbers5_combination([N1, N2, N3, N4, N5]) ->
    case check_numbers5([N1, N2, N3, N4, N5 + 1]) of
        true -> [N1, N2, N3, N4, N5 + 1];
        false -> get_next_numbers5_combination([N1, N2, N3, N4, N5 + 1])
    end.

%% TODO (std_string) : think about generalization this functionality
-spec check_numbers3(Numbers :: numbers()) -> boolean().
check_numbers3([N, N, _N3]) -> false;
check_numbers3([N, _N2, N]) -> false;
check_numbers3([_N1, N, N]) -> false;
check_numbers3([_N1, _N2, _N3]) -> true.

%% TODO (std_string) : think about generalization this functionality
-spec check_numbers5(Numbers :: numbers()) -> boolean().
check_numbers5([N, N, _N3, _N4, _N5]) -> false;
check_numbers5([N, _N2, N, _N4, _N5]) -> false;
check_numbers5([N, _N2, _N3, N, _N5]) -> false;
check_numbers5([N, _N2, _N3, _N4, N]) -> false;
check_numbers5([_N1, N, N, _N4, _N5]) -> false;
check_numbers5([_N1, N, _N3, N, _N5]) -> false;
check_numbers5([_N1, N, _N3, _N4, N]) -> false;
check_numbers5([_N1, _N2, N, N, _N5]) -> false;
check_numbers5([_N1, _N2, N, _N4, N]) -> false;
check_numbers5([_N1, _N2, _N3, N, N]) -> false;
check_numbers5([_N1, _N2, _N3, _N4, _N5]) -> true.