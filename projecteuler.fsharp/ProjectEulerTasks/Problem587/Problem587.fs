namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System

[<TestFixture>]
type Problem587() =

    [<Literal>]
    let Epsilon = 1e-9

    [<Literal>]
    let PI = Math.PI

    // It can be show, that this function is relation of areas (area of the concave triangle to area of the L-section), which depends on the central angle with value "angle"
    let areaRelation (angle: double) =
        (1.0 + angle - (PI / 2.0) + (angle |> cos) - (angle |> sin)) / (2.0 * (1.0 - (PI / 4.0)))

    // f(left) >= 0, f(right) <= 0
    let rec findRoot (left: double) (right: double) (f: double -> double) =
        let middle = (right + left) / 2.0
        match middle with
        | _ when middle |> f |> abs <= Epsilon -> middle
        | _ when (middle - left) |> abs <= Epsilon -> middle
        |_ when middle |> f < 0.0 -> findRoot left middle f
        | _ -> findRoot middle right f

    let solveImpl (areaRelationValue: double) =
        let angle = findRoot 0.0 (PI / 2.0) (fun angle -> (angle |> areaRelation) - areaRelationValue)
        let tanValue = (1.0 - (angle |> sin)) / (1.0 - (angle |> cos))
        1.0 / tanValue |> ceil |> int

    [<TestCase(0.1, 15, TimeThresholds.HardTimeLimit)>]
    [<TestCase(0.001, 2240, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(areaRelationValue: double, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, areaRelationValue)
