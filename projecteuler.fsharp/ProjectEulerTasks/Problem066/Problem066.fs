namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem066() =

    let solveImpl (maxNumber: int) =
        seq { 2 .. maxNumber } |>
        Seq.map (fun number -> number, PellEquation.FindFirstSolution(number, 1)) |>
        Seq.filter (fun (_, solution) -> solution.IsSome) |>
        Seq.maxBy (fun (_, solution) -> solution.Value.X) |> fst

    [<TestCase(7, 5, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 661, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)