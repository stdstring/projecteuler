namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem052() =

    let checkNumber (number: int) =
        let sourceDigits = number |> NumbersDigits.GetDigits |> List.sort
        seq {2 .. 6} |> Seq.exists (fun factor -> factor * number |> NumbersDigits.GetDigits |> List.sort = sourceDigits |> not) |> not

    let rec searchNumber (digitsCount: int) =
        let minNumber = pown 10 (digitsCount - 1)
        let maxNumber = ((pown 10 digitsCount) - 1) / 6
        match seq {minNumber .. maxNumber} |> Seq.tryFind checkNumber with
        | None -> searchNumber (digitsCount + 1)
        | Some number -> number

    let solveImpl () = searchNumber 1

    [<TestCase(142857, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)