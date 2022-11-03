namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem016() =

    let solveImpl (baseValue: int) (powerValue: int) =
        bigint.Pow(baseValue |> bigint, powerValue) |> NumbersDigits.GetDigits |> List.sum

    [<TestCase(2, 15, 26, TimeThresholds.HardTimeLimit)>]
    [<TestCase(2, 1000, 1366, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(baseValue: int, powerValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, baseValue, powerValue)
