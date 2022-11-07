namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem034() =

    let solveImpl () =
        // 7 * 9! == 2540160 - number with 7 digits; 8 * 9! == 2903040 - number with 7 digits, ...
        let digitFactorials = seq {0 .. 9} |> Seq.map (fun digit -> digit |> Numbers.CalcFactorial |> int) |> Seq.toArray
        let maxNumber = 7 * digitFactorials.[9]
        seq {3 .. maxNumber} |> Seq.filter (fun number -> number |> NumbersDigits.GetDigits |> Seq.map (fun digit -> digitFactorials.[digit]) |> Seq.sum = number) |> Seq.sum

    [<TestCase(40730, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)
