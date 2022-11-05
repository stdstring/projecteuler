namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

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