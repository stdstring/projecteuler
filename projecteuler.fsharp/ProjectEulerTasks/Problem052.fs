namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
// Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

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