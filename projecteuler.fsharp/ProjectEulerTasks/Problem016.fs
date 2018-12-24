namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26. What is the sum of the digits of the number 2^1000?

[<TestFixture>]
type Problem016() =

    let solveImpl (baseValue: int) (powerValue: int) =
        bigint.Pow(baseValue |> bigint, powerValue) |> Numbers.GetDigits |> List.sum

    [<TestCase(2, 15, 26, TimeThresholds.HardTimeLimit)>]
    [<TestCase(2, 1000, 1366, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(baseValue: int, powerValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl baseValue powerValue)
