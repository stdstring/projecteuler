namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils


// The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
// Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

[<TestFixture>]
type Problem048() =

    let power (baseValue: int64) (powerValue: int) (maxValue: int64) =
        seq {1 .. powerValue} |> Seq.fold (fun product _ -> (product * baseValue) % maxValue) 1L

    let solveImpl (maxNumber: int) (lastDigitsCount: int) =
        let maxValue = pown 10L lastDigitsCount
        let result = ((seq {1 .. maxNumber} |> Seq.map (fun number -> power (number |> int64) number maxValue) |> Seq.sum) % maxValue).ToString()
        ["0" |> String.replicate (lastDigitsCount - result.Length); result] |> String.concat ""

    [<TestCase(10, 10, "0405071317", TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 10, "9110846700", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, lastDigitsCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, lastDigitsCount)