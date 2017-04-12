%% @author std-string

%% It is easily proved that no equilateral triangle exists with integral length sides and integral area.
%% However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
%% We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.
%% Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

-module(problem_094).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{1000000000, 518408346}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(MaxPerimeter) ->
    MaxSide = round(MaxPerimeter / 3),
    process(2, MaxSide, MaxPerimeter, 0).

%% ====================================================================
%% Internal functions
%% ====================================================================

process(Side, MaxSide, _MaxPerimeter, Sum) when Side > MaxSide -> Sum;
process(Side, MaxSide, MaxPerimeter, Sum) ->
    Perimeter1 = check_triangle(Side, Side + 1, MaxPerimeter),
    Perimeter2 = check_triangle(Side, Side - 1, MaxPerimeter),
    process(Side + 1, MaxSide, MaxPerimeter, Sum + Perimeter1 + Perimeter2).

check_triangle(Side, OtherSide, MaxPerimeter) ->
    Perimeter = 2 * Side + OtherSide,
    if
        Perimeter > MaxPerimeter -> 0;
        Perimeter =< MaxPerimeter ->
            %% Square = sqrt((p/2)*(p/2-a)*(p/2-b)*(p/2-c)) = sqrt((p*(p-2a)*(p-2b)*(p-2c))/16) = (1/4)*sqrt(p*(p-2a)*(p-2b)*(p-2c))
            %% let a = b <> c => Square = (1/4)*sqrt(p*c*c*(p-2c)) = (c/4)*sqrt(p*(p-2c))
            SquareValue = Perimeter * (Perimeter - 2 * OtherSide),
            case check_square(SquareValue) of
                {true, Value} when (OtherSide * Value rem 4 == 0) -> Perimeter;
                _Other -> 0
            end
    end.

check_square(SquareValue) ->
    Value = math:sqrt(SquareValue),
    BottomValue = trunc(Value),
    TopValue = round(Value),
    if
        (SquareValue == BottomValue * BottomValue) -> {true, BottomValue};
        (SquareValue == TopValue * TopValue) -> {true, TopValue};
        true -> false
    end.