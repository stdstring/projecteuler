namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem010() =

    let solveImpl (maxNumber: int) =
        EratosSieve.Create(maxNumber).ToSeq() |> Seq.map int64 |> Seq.sum

    [<TestCase(10, 17L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1999999, 142913828922L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
