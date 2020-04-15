namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System

// A square is drawn around a circle as shown in the diagram below on the left.
// We shall call the blue shaded region the L-section.
// A line is drawn from the bottom left of the square to the top right as shown in the diagram on the right.
// We shall call the orange shaded region a concave triangle: see image here https://projecteuler.net/problem=587
// It should be clear that the concave triangle occupies exactly half of the L-section.
// Two circles are placed next to each other horizontally, a rectangle is drawn around both circles, and a line is drawn from the bottom left to the top right as shown in the diagram here https://projecteuler.net/problem=587.
// This time the concave triangle occupies approximately 36.46% of the L-section.
// If n circles are placed next to each other horizontally, a rectangle is drawn around the n circles, and a line is drawn from the bottom left to the top right,
// then it can be shown that the least value of n for which the concave triangle occupies less than 10% of the L-section is n = 15.
// What is the least value of n for which the concave triangle occupies less than 0.1% of the L-section?

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
