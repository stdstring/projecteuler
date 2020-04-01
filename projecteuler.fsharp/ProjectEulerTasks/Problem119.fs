namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// The number 512 is interesting because it is equal to the sum of its digits raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a number with this property is 614656 = 28^4.
// We shall define a(n) to be the nth term of this sequence and insist that a number must contain at least two digits to have a sum.
// You are given that a(2) = 512 and a(10) = 614656. Find a(30).

module Problem119Impl =
    type PowerData = {Sum: int64; Number: int64}

open Problem119Impl


[<TestFixture>]
type Problem119() =

    let generateValuePowers (maxValue: int64) (number: int64) =
        let unfoldFun (value: int64) =
            match value > maxValue with
            | true -> None
            | false -> ({PowerData.Sum = number; PowerData.Number = value}, value * number) |> Some
        number * number |> Seq.unfold unfoldFun

    let solveImpl (n: int) =
        // Int64.MaxValue = 9,223,372,036,854,775,807
        let maxValue = 999999999999999999L
        let maxDigitSum = maxValue |> NumbersDigits.GetDigits |> Seq.sum |> int64
        let data = seq {2L .. maxDigitSum} |>
                   Seq.map (fun number -> number |> generateValuePowers (maxValue / number)) |>
                   Seq.concat |>
                   Seq.filter (fun data -> data.Number > 9L && data.Sum = (data.Number |> NumbersDigits.GetDigits |> Seq.sum |> int64)) |>
                   Seq.sortBy (fun data -> data.Number) |>
                   Seq.toArray
        data.[n - 1].Number

    [<TestCase(2, 512L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10, 614656L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(30, 248155780267521L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(n: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, n)