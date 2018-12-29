namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
// 1634 = 1^4 + 6^4 + 3^4 + 4^4
// 8208 = 8^4 + 2^4 + 0^4 + 8^4
// 9474 = 9^4 + 4^4 + 7^4 + 4^4
// As 1 = 1^4 is not a sum it is not included.
// The sum of these numbers is 1634 + 8208 + 9474 = 19316.
// Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

[<TestFixture>]
type Problem030() =

    let rec findInfimum (value: int) (sum: int) (delta: int) =
        match value with
        | _ when value < sum -> findInfimum (value * 10) (sum + delta) delta
        | _ -> sum

    let solveImpl (power: int) =
        let delta = pown 9 power
        let infimum = findInfimum 1 delta delta
        seq {2 .. infimum} |> Seq.filter (fun number -> number |> NumbersDigits.GetDigits |> Seq.map (fun(digit) -> pown digit power) |> Seq.sum = number) |> Seq.sum

    [<TestCase(4, 19316, TimeThresholds.HardTimeLimit)>]
    [<TestCase(5, 443839, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(power: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, power)
