namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem056() =

    let solveImpl (maxBase: int) (maxPower: int) =
        seq { for baseValue in 2 .. maxBase do for powerValue in 2 .. maxPower do yield baseValue, powerValue } |>
        Seq.map (fun (baseValue, powerValue) -> pown (baseValue |> bigint) powerValue |> NumbersDigits.GetDigits |> List.sum) |>
        Seq.max

    [<TestCase(100, 100, 972, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxBase: int, maxPower: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxBase, maxPower)