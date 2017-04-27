%% @author std-string

%% In laser physics, a "white cell" is a mirror system that acts as a delay line for the laser beam.
%% The beam enters the cell, bounces around on the mirrors, and eventually works its way back out.
%% The specific white cell we will be considering is an ellipse with the equation 4*x^2 + y^2 = 100
%% The section corresponding to -0.01 <= x <= +0.01 at the top is missing, allowing the light to enter and exit through the hole.
%% The light beam in this problem starts at the point (0.0, 10.1) just outside the white cell, and the beam first impacts the mirror at (1.4, -9.6).
%% Each time the laser beam hits the surface of the ellipse, it follows the usual law of reflection "angle of incidence equals angle of reflection."
%% That is, both the incident and reflected beams make the same angle with the normal line at the point of incidence.
%% The slope m of the tangent line at any point (x, y) of the given ellipse is: m = − 4 * x / y
%% The normal line is perpendicular to this tangent line at the point of incidence.
%% How many times does the beam hit the internal surface of the white cell before exiting?

-module(problem_144).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(LEFT_HOLE_X_BORDER, -0.01).
-define(RIGHT_HOLE_X_BORDER, 0.01).

-define(POINT_0, {0.0, 10.1}).
-define(POINT_1, {1.4, -9.6}).

%% TODO (std_string) : move into common
-type point() :: {X :: float(), Y :: float()}.
-type vector() :: {X :: float(), Y :: float()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 354}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) ->
    {X0, Y0} = ?POINT_0,
    {X1, Y1} = ?POINT_1,
    Length = math:sqrt((X1 - X0) * (X1 - X0) + (Y1 - Y0) * (Y1 - Y0)),
    Direct0 = {(X1 - X0) / Length, (Y1 - Y0) / Length},
    Normal1 = calc_normal(?POINT_1),
    Direct1 = reflect_vector(Direct0, Normal1),
    {_LastPoint, _LastDirect, ReflectNumber} = propagate_beam(?POINT_1, Direct1, 1),
    ReflectNumber.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec propagate_beam(StartPoint :: point(), DirectVector :: vector(), ReflectNumber :: pos_integer()) ->
    {LastPoint :: point(), LastDirectVector :: vector(), ReflectNumber :: pos_integer()}.
propagate_beam({StartX, StartY}, {DirectX, DirectY}, ReflectNumber) ->
    FinishPoint = calc_crosspoint({StartX, StartY}, {DirectX, DirectY}),
    case is_hole(FinishPoint) of
        true -> {{StartX, StartY}, {DirectX, DirectY}, ReflectNumber};
        false ->
            FinishNormal = calc_normal(FinishPoint),
            NewDirect = reflect_vector({DirectX, DirectY}, FinishNormal),
            propagate_beam(FinishPoint, NewDirect, ReflectNumber + 1)
    end.

-spec calc_normal(Point :: point()) -> vector().
calc_normal({X0, Y0}) ->
    %% ellipse equation is 4*x^2 + y^2 = 100, (x^2 / 25) + (y^2 / 100) = 1 => a^2 = 25, b^2 = 100
    %% tangent equation at point is (x0, y0): (x0 * x / a^2) + (y0 * y / b^2) = 1
    %% in our case, tangent equation is (x0 * x / 25) + (y0 * y / 100) = 1 => 4 * x0 *x + y0 * y - 100 = 0
    %% in general line equation A * x + B * y + C = 0 vector (A, B) is normal to line
    %% in our case normal vector (not normalized) is (4*x0, y0)
    %% because (4*x0, y0) * (x0, y0) = 4 * x0^2 + y0^2 > 0 for any x0, y0 => (4*x0, y0) - external normal vector (directed from ellipse)
    %% we need in internal normal => we choose(-4*x0, -y0) as normal vector
    Length = math:sqrt(16 * X0 * X0 + Y0 * Y0),
    {-4 * X0 / Length, -Y0 / Length}.

-spec reflect_vector(SourceBeamVector :: vector(), Normal :: vector()) -> vector().
reflect_vector({SourceX, SourceY}, {NormalX, NormalY}) ->
    %% inverse source vector
    AngleCos = -1.0 * (SourceX * NormalX + SourceY * NormalY),
    AngleSin = math:sqrt(1 - AngleCos * AngleCos),
    %% for calculate dest vector we need rotate normal on angle between normal and source vector (this rotation is counterclockwise)
    DestX = NormalX * AngleCos - NormalY * AngleSin,
    DestY = NormalX * AngleSin + NormalY * AngleCos,
    %% probably this normalization is redundant
    Length = math:sqrt(DestX * DestX + DestY * DestY),
    {DestX / Length, DestY / Length}.

-spec calc_crosspoint(StartPoint :: point(), DirectVector :: vector()) -> point().
calc_crosspoint({StartX, StartY}, {DirectX, DirectY}) ->
    %% ellipse equation is 4 * x^2 + y^2 = 100
    %% line parametric equation is x = x0 + dx * t, y = y0 + dy * t
    %% substitution into the ellipse equation:
    %% 4 * (x0 + dx * t)^2 + (y0 + dy * t)^2 = 100
    %% 4 * (x0^2 + 2 * x0 * dx * t + dx^2 * t^2) + (y0^2 + 2 * y0 * dy * t + dy^2 * t^2) = 100
    %% (4 * x0^2 + y0^2) + 4 * (2 * x0 * dx * t + dx^2 * t^2) + (2 * y0 * dy * t + dy^2 * t^2) = 100
    %% 4 * x0^2 + y0^2 = 100 => 4 * (2 * x0 * dx * t + dx^2 * t^2) + (2 * y0 * dy * t + dy^2 * t^2) = 0
    %% 8 * x0 * dx * t + 4 * dx^2 * t^2 + 2 * y0 * dy * t + dy^2 * t^2 = 0
    %% t * (8 * x0 * dx + 4 * dx^2 * t + 2 * y0 * dy + dy^2 * t) = 0, t != 0
    %% 8 * x0 * dx + 4 * dx^2 * t + 2 * y0 * dy + dy^2 * t = 0
    %% (8 * x0 * dx + 2 * y0 * dy) + t * (4 * dx^2 + dy^2) = 0
    %% t = - (8 * x0 * dx + 2 * y0 * dy) / (4 * dx^2 + dy^2)
    %% PS. we expect that t > 0
    %% dx^2 + dy^2 = 1 => t = - 2 * (4 * x0 * dx + y0 * dy) / (1 + 3 * dx^2)
    TValue = - 2 * (4 * StartX * DirectX + StartY * DirectY) / (1 + 3 * DirectX * DirectX),
    CrossX = StartX + DirectX * TValue,
    CrossY = StartY + DirectY * TValue,
    {CrossX, CrossY}.

-spec is_hole(Point :: point()) -> boolean().
is_hole({X, Y}) ->
    (Y > 0) and (?LEFT_HOLE_X_BORDER =< X) and (X =< ?RIGHT_HOLE_X_BORDER).