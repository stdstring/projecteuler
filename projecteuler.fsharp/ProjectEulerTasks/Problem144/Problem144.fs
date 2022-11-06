namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// TODO (std_string) : probably, move into CommonLib
module Problem144Impl =
    type Point = {X: float; Y: float}
    type Vector = {X: float; Y: float}

open Problem144Impl

[<TestFixture>]
type Problem144() =

    let holeLeftXBorder = -0.01
    let holeRightXBorder = 0.01

    let point0 = {Point.X = 0.0; Point.Y = 10.1}
    let point1 = {Point.X = 1.4; Point.Y = -9.6}

    let calcNormal (point: Point) =
        // ellipse equation is 4*x^2 + y^2 = 100, (x^2 / 25) + (y^2 / 100) = 1 => a^2 = 25, b^2 = 100
        // tangent equation at point is (x0, y0): (x0 * x / a^2) + (y0 * y / b^2) = 1
        // in our case, tangent equation is (x0 * x / 25) + (y0 * y / 100) = 1 => 4 * x0 *x + y0 * y - 100 = 0
        // in general line equation A * x + B * y + C = 0 vector (A, B) is normal to line
        // in our case normal vector (not normalized) is (4*x0, y0)
        // because (4*x0, y0) * (x0, y0) = 4 * x0^2 + y0^2 > 0 for any x0, y0 => (4*x0, y0) - external normal vector (directed from ellipse)
        // we need in internal normal => we choose(-4*x0, -y0) as normal vector
        let length = 16.0 * point.X * point.X + point.Y * point.Y |> sqrt
        {Vector.X = -4.0 * point.X / length; Vector.Y = -point.Y / length}

    let reflectVector (sourceVector :Vector) (normal :Vector) =
        // inverse source vector
        let angleCos = -1.0 * (sourceVector.X * normal.X + sourceVector.Y * normal.Y)
        let angleSin = 1.0 - angleCos * angleCos |> sqrt
        // for calculate dest vector we need rotate normal on angle between normal and source vector (this rotation is counterclockwise)
        let destX = normal.X * angleCos - normal.Y * angleSin
        let destY = normal.X * angleSin + normal.Y * angleCos
        // probably this normalization is redundant
        let length = destX * destX + destY * destY |> sqrt
        {Vector.X = destX / length; Vector.Y = destY / length}

    let calcCrosspoint (startPoint: Point) (directVector :Vector) =
        // ellipse equation is 4 * x^2 + y^2 = 100
        // line parametric equation is x = x0 + dx * t, y = y0 + dy * t
        // substitution into the ellipse equation:
        // 4 * (x0 + dx * t)^2 + (y0 + dy * t)^2 = 100
        // 4 * (x0^2 + 2 * x0 * dx * t + dx^2 * t^2) + (y0^2 + 2 * y0 * dy * t + dy^2 * t^2) = 100
        // (4 * x0^2 + y0^2) + 4 * (2 * x0 * dx * t + dx^2 * t^2) + (2 * y0 * dy * t + dy^2 * t^2) = 100
        // 4 * x0^2 + y0^2 = 100 => 4 * (2 * x0 * dx * t + dx^2 * t^2) + (2 * y0 * dy * t + dy^2 * t^2) = 0
        // 8 * x0 * dx * t + 4 * dx^2 * t^2 + 2 * y0 * dy * t + dy^2 * t^2 = 0
        // t * (8 * x0 * dx + 4 * dx^2 * t + 2 * y0 * dy + dy^2 * t) = 0, t != 0
        // 8 * x0 * dx + 4 * dx^2 * t + 2 * y0 * dy + dy^2 * t = 0
        // (8 * x0 * dx + 2 * y0 * dy) + t * (4 * dx^2 + dy^2) = 0
        // t = - (8 * x0 * dx + 2 * y0 * dy) / (4 * dx^2 + dy^2)
        // PS. we expect that t > 0
        // dx^2 + dy^2 = 1 => t = - 2 * (4 * x0 * dx + y0 * dy) / (1 + 3 * dx^2)
        let tValue = -2.0 * (4.0 * startPoint.X * directVector.X + startPoint.Y * directVector.Y) / (1.0 + 3.0 * directVector.X * directVector.X)
        let crossX = startPoint.X + directVector.X * tValue
        let crossY = startPoint.Y + directVector.Y * tValue
        {Point.X = crossX; Point.Y = crossY}

    let isHole (point: Point) = (point.Y > 0.0) && (holeLeftXBorder <= point.X) && (point.X <= holeRightXBorder)

    let solveImpl () =
        let rec propagateBeam (startPoint :Point) (directVector :Vector) (reflectNumber :int) =
            let finishPoint = calcCrosspoint startPoint directVector
            match finishPoint |> isHole with
            | true -> reflectNumber
            | false ->
                let finishNormal = calcNormal finishPoint
                let newDirectVector = reflectVector directVector finishNormal
                propagateBeam finishPoint newDirectVector (reflectNumber + 1)
        let length = (point1.X - point0.X) * (point1.X - point0.X) + (point1.Y - point0.Y) * (point1.Y - point0.Y) |> sqrt
        let direct0 = {Vector.X = (point1.X - point0.X) / length; Vector.Y = (point1.Y - point0.Y) / length}
        let normal1 = calcNormal point1
        let direct1 = reflectVector direct0 normal1
        propagateBeam point1 direct1 1

    [<TestCase(354, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)