namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem069() =

    let solveImpl (maxNumber: int) =
        let eulerTotientFunction = maxNumber |> EulerTotientFunction.Create
        seq {2 .. maxNumber} |> Seq.maxBy (fun number -> (number |> float) / (number |> eulerTotientFunction.GetValue |> float))

    [<TestCase(10, 6, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 510510, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
