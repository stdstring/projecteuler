namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one followed by two-hundred zeros.
// Despite their size, the sum of the digits in each number is only 1.
// Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

[<TestFixture>]
type Problem056() =

    let solveImpl (maxBase: int) (maxPower: int) =
        seq { for baseValue in 2 .. maxBase do for powerValue in 2 .. maxPower do yield baseValue, powerValue } |>
        Seq.map (fun (baseValue, powerValue) -> pown (baseValue |> bigint) powerValue |> NumbersDigits.GetDigits |> List.sum) |>
        Seq.max

    [<TestCase(100, 100, 972, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxBase: int, maxPower: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxBase, maxPower)