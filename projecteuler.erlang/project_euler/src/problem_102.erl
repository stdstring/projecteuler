%% @author std-string

%% Three distinct points are plotted at random on a Cartesian plane, for which -1000 ≤ x, y ≤ 1000, such that a triangle is formed.
%% Consider the following two triangles:
%% A(-340,495), B(-153,-910), C(835,-947)
%% X(-175,41), Y(-421,-714), Z(574,-645)
%% It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.
%% Using problem_012.dat, a data file containing the co-ordinates of one thousand "random" triangles, find the number of triangles for which the interior contains the origin.
%% NOTE: The first two examples in the file represent the triangles in the example given above.

-module(problem_102).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_102.dat", 228}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    load_utils:read_number_table(filename:join(ModuleSourceDir, Filename), ",").

-spec solve(PreparedInput :: term()) -> term().
solve(VertexTable) ->
    VertexTable,
    CollectFun = fun(Row, Count) ->
        case check_triangle(Row) of
            true -> Count + 1;
            false -> Count
        end
    end,
    lists:foldl(CollectFun, 0, VertexTable).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_triangle(Triangle :: [integer()]) -> boolean().
check_triangle([X1, Y1, X2, Y2, X3, Y3]) ->
    ProductZ1 = claclulate_vectorz_product(X1, Y1, X2, Y2),
    ProductZ2 = claclulate_vectorz_product(X2, Y2, X3, Y3),
    ProductZ3 = claclulate_vectorz_product(X3, Y3, X1, Y1),
    check_vectorz_products(ProductZ1, ProductZ2, ProductZ3).

-spec claclulate_vectorz_product(X1 :: integer(), Y1 :: integer(), X2 :: integer(), Y2 :: integer()) -> integer().
claclulate_vectorz_product(X1, Y1, X2, Y2) -> Y1 * X2 - X1 * Y2.

-spec check_vectorz_products(ProductZ1 :: integer(), ProductZ2 :: integer(), ProductZ3 :: integer()) -> boolean().
check_vectorz_products(ProductZ1, ProductZ2, ProductZ3) when ProductZ1 > 0, ProductZ2 > 0, ProductZ3 > 0 -> true;
check_vectorz_products(ProductZ1, ProductZ2, ProductZ3) when ProductZ1 < 0, ProductZ2 < 0, ProductZ3 < 0 -> true;
check_vectorz_products(_ProductZ1, _ProductZ2, _ProductZ3) -> false.