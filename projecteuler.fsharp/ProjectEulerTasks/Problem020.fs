namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// n! means n * (n - 1) * ... * 3 * 2 * 1
// For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800, and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
// Find the sum of the digits in the number 100!

[<TestFixture>]
type Problem020() =

    let solveImpl (number: int) =
        number |> Numbers.CalcFactorial |> NumbersDigits.GetDigits |> Seq.sum

    [<TestCase(10, 27, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 648, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(number: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, number)
