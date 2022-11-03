namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem020() =

    let solveImpl (number: int) =
        number |> Numbers.CalcFactorial |> NumbersDigits.GetDigits |> Seq.sum

    [<TestCase(10, 27, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 648, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(number: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, number)
