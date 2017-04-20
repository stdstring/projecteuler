%% @author std-string

%% Find the greatest product of five consecutive digits in the 1000-digit number.
%%
%% 73167176531330624919225119674426574742355349194934
%% 96983520312774506326239578318016984801869478851843
%% 85861560789112949495459501737958331952853208805511
%% 12540698747158523863050715693290963295227443043557
%% 66896648950445244523161731856403098711121722383113
%% 62229893423380308135336276614282806444486645238749
%% 30358907296290491560440772390713810515859307960866
%% 70172427121883998797908792274921901699720888093776
%% 65727333001053367881220235421809751254540594752243
%% 52584907711670556013604839586446706324415722155397
%% 53697817977846174064955149290862569321978468622482
%% 83972241375657056057490261407972968652414535100474
%% 82166370484403199890008895243450658541227588666881
%% 16427171479924442928230863465674813919123162824586
%% 17866458359124566529476545682848912883142607690042
%% 24219022671055626321111109370544217506941658960408
%% 07198403850962455444362981230987879927244284909188
%% 84580156166097919133875499200524063689912560717606
%% 05886116467109405077541002256983155200055935729725
%% 71636269561882670428252483600823257530420752963450

-module(problem_008).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type digit() :: 0..9.
-type digits() :: [digit()].

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{{"problem_008.dat", 5}, 40824}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, {Filename, Count}) ->
    {ok, BinaryData} = file:read_file(filename:join(ModuleSourceDir, Filename)),
    Digits = lists:map(fun(Char) -> Char - $0 end, binary_to_list(BinaryData)),
    {Digits, Count}.

-spec solve(PreparedInput :: term()) -> term().
solve({Digits, Count}) -> find_max_product(Digits, Count).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec find_max_product(Digits :: digits(), Count :: pos_integer()) -> non_neg_integer().
find_max_product(Digits, Count) -> find_max_product(Digits, 1, Count, length(Digits), 0).

-spec find_max_product(Digits :: digits(),
                       Index :: pos_integer(),
                       Count :: pos_integer(),
                       Size :: pos_integer(),
                       MaxProduct :: non_neg_integer()) -> non_neg_integer().
find_max_product(_Digits, Index, Count, Size, MaxProduct) when Index + Count > Size -> MaxProduct;
find_max_product([_Digit | DigitsRest] = Digits, Index, Count, Size, MaxProduct) ->
    PretenderProduct = calc_product(Digits, Count),
    if
        PretenderProduct > MaxProduct -> find_max_product(DigitsRest, Index + 1, Count, Size, PretenderProduct);
        true -> find_max_product(DigitsRest, Index + 1, Count, Size, MaxProduct)
    end.

-spec calc_product(Digits :: digits(), Count :: pos_integer()) -> non_neg_integer().
calc_product(Digits, Count) ->
    lists:foldl(fun(Digit, Product) -> Digit * Product end, 1, lists:sublist(Digits, Count)).