%% @author std-string

%% The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin, O(0,0), to form ΔOPQ.
%% There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies between 0 and 2 inclusive;
%% that is, 0 ≤ x1, y1, x2, y2 ≤ 2 (see https://projecteuler.net/problem=91).
%% Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?

-module(problem_091).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{{2, 2}, 14}, {{50, 50}, 14234}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxX, MaxY}) ->
    calc_c1_count(MaxX, MaxY) + calc_c2_count(MaxX, MaxY).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Math:
%% Let 0 ≤ x1, x2 ≤ MaxX, 0 ≤ y1, y2 ≤ MaxY
%% Let C1 - count of right triangles with legs, which are parallel to OX and OY axes
%% Let C2 - count of right triangles with legs, which are not parallel to OX and OY axes
%% Then total count of triangles = C1 + C2
%% Obviously, that C1 = 3 * MaxX * MaxY
%% Let (A, B) - arbitrary point in 0 ≤ A ≤ MaxX, 0 ≤ B ≤ MaxY with integer coordinates; R = (A, B) - radius vector of this point
%% Let N - vector perpendicular to R, then N = (-B, A) (or N = (B, -A))
%% Then equation of the line with the directing vector N and passing through the point (A, B) will be the following:
%% x = A - B * t / L, y = B + A * t / L, where t - parameter, L = sqrt(A^2 + B^2) - length of the vector N
%% t = - (x - A) * L / B => y = B - A * (X - A) / B
%% if y - integer, then A * (X - A) rem B = 0 => A * (X - A) = k * B, where k - integer (k = ..., -2, -1, 0, 1, 2, ...)
%% Let gcd(A, B) = N => B / A = B' / A' => x = A + k * B' / A' => k = ..., -2 * A', -A', 0, A', 2 * A', ...
%% y = B - A * (X - A) / B = B - A' * (X - A) / B' = B - A' * k * B' / (B' * A') = B - k
%% So, we can build the right triangles with integer coordinates with the following vertices:
%% 1) (0, 0), 2) (A, B), 3) (A + k * B' / A', B - k) k = ..., -2 * A', -A', 0, A', 2 * A', ...
%% Doing so for all possible values of A, B, k, we can calculate C2

-spec calc_c1_count(MaxX :: pos_integer(), MaxY :: pos_integer()) -> pos_integer().
calc_c1_count(MaxX, MaxY) -> 3 * MaxX * MaxY.

-spec calc_c2_count(MaxX :: pos_integer(), MaxY :: pos_integer()) -> pos_integer().
calc_c2_count(MaxX, MaxY) -> calc_c2_count_impl(1, 1, MaxX, MaxY, 0).

-spec calc_c2_count_impl(X :: pos_integer(),
                         Y :: pos_integer(),
                         MaxX :: pos_integer(),
                         MaxY :: pos_integer(),
                         Count :: non_neg_integer()) -> non_neg_integer().
calc_c2_count_impl(MaxX, MaxY, MaxX, MaxY, Count) -> Count;
calc_c2_count_impl(X, Y, MaxX, MaxY, Count) when X > MaxX-> calc_c2_count_impl(1, Y + 1, MaxX, MaxY, Count);
calc_c2_count_impl(CurrentX, CurrentY, MaxX, MaxY, Count) ->
    calc_c2_count_impl(CurrentX + 1, CurrentY, MaxX, MaxY, Count + calc_c2_case_count(CurrentX, CurrentY, MaxX, MaxY)).

-spec calc_c2_case_count(A :: pos_integer(), B :: pos_integer(), MaxX :: pos_integer(), MaxY :: pos_integer()) -> non_neg_integer().
calc_c2_case_count(A, B, MaxX, MaxY) ->
    GcdValue = number_dividers:calc_gcd(A, B),
    SimpleA = A div GcdValue,
    SimpleB = B div GcdValue,
    min((MaxX - A) div SimpleB, B div SimpleA) + min(A div SimpleB, (MaxY - B) div SimpleA).