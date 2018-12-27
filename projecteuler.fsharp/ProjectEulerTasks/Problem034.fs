namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
// Find the sum of all numbers which are equal to the sum of the factorial of their digits.
// Note: as 1! = 1 and 2! = 2 are not sums they are not included.

[<TestFixture>]
type Problem034() =

    let solveImpl () =
        // 7 * 9! == 2540160 - number with 7 digits; 8 * 9! == 2903040 - number with 7 digits, ...
        let digitFactorials = seq {0 .. 9} |> Seq.map (fun digit -> digit |> Numbers.CalcFactorial |> int) |> Seq.toArray
        let maxNumber = 7 * digitFactorials.[9]
        seq {3 .. maxNumber} |> Seq.filter (fun number -> number |> NumbersDigits.GetDigits |> Seq.map (fun digit -> digitFactorials.[digit]) |> Seq.sum = number) |> Seq.sum

    [<TestCase(40730, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl ())