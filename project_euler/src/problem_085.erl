%% @author std-string

%% By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles (see https://projecteuler.net/problem=65).
%% Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.

-module(problem_085).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type search_result() :: {Area :: pos_integer(), RectanglesCount :: pos_integer()}.
-type rectangles_count_generator() :: fun((Width :: pos_integer()) -> pos_integer()).
-type next_border_generator() :: fun((RightBorder :: pos_integer()) -> pos_integer()).
-type search_range() ::{LeftBorder :: pos_integer(), RightBorder :: pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{2000000, 2772}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(RectanglesCount) ->
    MinArea = find_bottom_eval(RectanglesCount),
    MaxArea = find_top_eval(RectanglesCount),
    find_nearest_value(MinArea, MaxArea, RectanglesCount).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Math:
%% Let we have the rectangle N * M, where min(N, M) = M; S = N * M - area value of this rectangle
%% Let C(A, B) - quantity of the unique rectangles A * B, which can be situated inside the source rectangle N * M (unique ?)
%% Let S(N) - sum of an arithmetic progression from 1 to N
%% Than we have the following relations:
%% C(1, 1) = N * M
%% C(2, 1) = (N - 1) * M
%% C(3, 1) = (N - 2) * M
%% ...
%% C(N, 1) = 1 * M
%% C(1, 1) + C(2, 1) + ... + C(N, 1) = M * (N + (N - 1) + ... + 1) = M * S(N) - total quantity of unique rectangles kind A * 1 where A = 1 .. N
%% C(2, 1) = N * (M - 1)
%% C(2, 2) = (N - 1) * (M - 1)
%% ...
%% C(N, 2) = 1 * (M - 1)
%% C(2, 1) + C(2, 2) + ... + C(N, 2) = (M - 1) * (N + (N - 1) + ... + 1) = (M - 1) * S(N) - total quantity of unique rectangles kind A * 2 where A = 1 .. N
%% ...
%% M * S(N) + (M - 1) * S(N) + ... 1 * S(N) = S(N) * (M + (M - 1) + ... + 1) = S(N) * S(M) - total quantity of all unique rectangles
%% S(N) = N * (N + 1) / 2 = (N^2 + N) / 2
%% S(N) * S(M) = (N^2 + N) * (M^2 + M) / 4 = (N^2 * M^2 + N * M^2 + N^2 * M + N * M) / 4 = (S^2 + S * (N + M + 1)) / 4
%% We can see, that S(N) * S(M) = A * (N + M) + B, where A, B - some constants for some giving area value S
%% Since S = N * M => S(N) * S(M) = A * (N + S / N) + B
%% Let we see the following function f(X) = A * (X + S / X) + B
%% f'(X) = A * (1 - S / X^2), consider f'(X) = 0 => 1 - S / X^2 = 0 => S / X^2 = 1 => X^2 = S => X = S^(1/2)
%% Anyone can see, that at X = S^(1/2) function f(X) has the minimum.
%% And anyone can see, that at X = S (or at X = 1) function f(X) has the maximum on [1, S] segment.

-spec find_nearest_value(MinArea :: pos_integer(), MaxArea :: pos_integer(), RectanglesCount :: pos_integer()) -> pos_integer().
find_nearest_value(MinArea, MaxArea, RectanglesCount) -> find_nearest_value_impl(MinArea, MaxArea, RectanglesCount, {0, 0}).

-spec find_nearest_value_impl(CurrentArea :: pos_integer(),
                              MaxArea :: pos_integer(),
                              RectanglesCount :: pos_integer(),
                              SearchResult :: search_result()) -> pos_integer().
find_nearest_value_impl(CurrentArea, MaxArea, _RectanglesCount, {SavedArea, _SavedRectanglesCount}) when CurrentArea > MaxArea -> SavedArea;
find_nearest_value_impl(CurrentArea, MaxArea, RectanglesCount, SearchResult) ->
    find_nearest_value_impl(CurrentArea + 1, MaxArea, RectanglesCount, find_nearest_rectangle_value(CurrentArea, RectanglesCount, SearchResult)).

-spec find_nearest_rectangle_value(CurrentArea :: pos_integer(),
                                   RectanglesCount :: pos_integer(),
                                   SearchResult :: search_result()) -> search_result().
find_nearest_rectangle_value(CurrentArea, RectanglesCount, SearchResult) ->
    find_nearest_rectangle_value_impl(number_dividers:get_dividers(CurrentArea), RectanglesCount, SearchResult).

-spec find_nearest_rectangle_value_impl(DividerRest :: [pos_integer()],
                                        RectanglesCount :: pos_integer(),
                                        SearchResult :: search_result()) -> search_result().
find_nearest_rectangle_value_impl([], _RectanglesCount, SearchResult) -> SearchResult;
find_nearest_rectangle_value_impl([Width | DividerRest], RectanglesCount, SearchResult) ->
    NewSearchResult = lists:foldl(fun(Height, {SavedArea, SavedRectanglesCount}) ->
        CurrentRectanglesCount = calc_rectangles_count(Width, Height),
        if
            abs(SavedRectanglesCount - RectanglesCount) =< abs(CurrentRectanglesCount - RectanglesCount) -> {SavedArea, SavedRectanglesCount};
            true -> {Width * Height, CurrentRectanglesCount}
        end
    end, SearchResult, DividerRest),
    find_nearest_rectangle_value_impl(DividerRest, RectanglesCount, NewSearchResult).

-spec calc_rectangles_count(M :: pos_integer(), N :: pos_integer()) -> pos_integer().
calc_rectangles_count(M, N) ->
    Area = M * N,
    Area * (Area + M + N + 1) div 4.

-spec find_bottom_eval(RectanglesCount :: pos_integer()) -> pos_integer().
find_bottom_eval(RectanglesCount) ->
    {LeftBorderWidth, RightBorderWidth} = find_line_search_range(RectanglesCount),
    LineMinWidth = find_line_bottom_value(RectanglesCount, LeftBorderWidth, RightBorderWidth),
    LineMinWidth * 1.

-spec find_line_search_range(RectanglesCount :: pos_integer()) -> search_range().
find_line_search_range(RectanglesCount) ->
    find_search_range(RectanglesCount, 1, 10, fun(Width) -> calc_rectangles_count(Width, 1) end, fun(RightBorder) -> RightBorder * 10 end).

-spec find_line_bottom_value(RectanglesCount :: pos_integer(), LeftBorder :: pos_integer(), RightBorder :: pos_integer()) -> pos_integer().
find_line_bottom_value(_RectanglesCount, Border, Border) -> Border;
find_line_bottom_value(RectanglesCount, LeftBorder, RightBorder) when (LeftBorder + 1) == RightBorder ->
    LeftRectanglesCount = calc_rectangles_count(LeftBorder, 1),
    if
        LeftRectanglesCount >= RectanglesCount -> LeftBorder;
        true -> RightBorder
    end;
find_line_bottom_value(RectanglesCount, LeftBorder, RightBorder) ->
    MiddleBorder = (RightBorder + LeftBorder) div 2,
    MiddleRectanglesCount = calc_rectangles_count(MiddleBorder, 1),
    if
        MiddleRectanglesCount < RectanglesCount -> find_line_bottom_value(RectanglesCount, MiddleBorder, RightBorder);
        MiddleRectanglesCount > RectanglesCount -> find_line_bottom_value(RectanglesCount, LeftBorder, MiddleBorder);
        MiddleRectanglesCount == RectanglesCount -> MiddleBorder
    end.

-spec find_top_eval(RectanglesCount :: pos_integer()) -> pos_integer().
find_top_eval(RectanglesCount) ->
    {_LeftBorderWidth, RightBorderWidth} = find_square_search_range(RectanglesCount),
    RightBorderWidth * RightBorderWidth.

-spec find_square_search_range(RectanglesCount :: pos_integer()) -> search_range().
find_square_search_range(RectanglesCount) ->
    find_search_range(RectanglesCount, 1, 2, fun(Width) -> calc_rectangles_count(Width, Width) end, fun(RightBorder) -> RightBorder + 1 end).

-spec find_search_range(RectanglesCount :: pos_integer(),
                        LeftBorder :: pos_integer(),
                        RightBorder :: pos_integer(),
                        RectanglesCountGenerator :: rectangles_count_generator(),
                        NextBorderGenerator :: next_border_generator()) -> search_range().
find_search_range(RectanglesCount, LeftBorder, RightBorder, RectanglesCountGenerator, NextBorderGenerator) ->
    LeftRectanglesCount = RectanglesCountGenerator(LeftBorder),
    RightRectanglesCount = RectanglesCountGenerator(RightBorder),
    if
        LeftRectanglesCount < RectanglesCount, RightRectanglesCount > RectanglesCount -> {LeftBorder, RightBorder};
        LeftRectanglesCount == RectanglesCount -> {LeftBorder, LeftBorder};
        RightRectanglesCount == RectanglesCount -> {RightBorder, RightBorder};
        true -> find_search_range(RectanglesCount, RightBorder, NextBorderGenerator(RightBorder), RectanglesCountGenerator, NextBorderGenerator)
    end.