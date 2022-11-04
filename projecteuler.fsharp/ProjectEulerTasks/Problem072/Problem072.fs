namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem072() =

    let solveImpl (maxNumber: int) =
        let eulerTotientFunction = maxNumber |> EulerTotientFunction.Create
        seq {2 .. maxNumber} |> Seq.map (fun number -> number |> eulerTotientFunction.GetValue |> int64) |> Seq.sum

    [<TestCase(8, 21L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 303963552391L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
