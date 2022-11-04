namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem038() =

    let createConcatProduct (number: int) =
        let rec createConcatProductImpl (factor: int) (digits: List<int>) =
            match digits with
            | _ when digits.Length > 9 -> None
            | _ when digits.Length = 9 -> digits |> NumbersDigits.GetNumber |> Some
            | _ -> createConcatProductImpl (factor + 1) (digits @ NumbersDigits.GetDigits(number * factor))
        createConcatProductImpl 1 []

    let solveImpl () =
        seq {9 .. 10000} |> Seq.choose createConcatProduct |> Seq.filter (fun number -> number |> PandigitalNumbers.IsPandigital) |> Seq.max |> int

    [<TestCase(932718654, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)
