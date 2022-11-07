namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem006() =

    let arithProgressionSum (a1: int) (an: int) (n :int) =
        n * (a1 + an) / 2

    let solveImpl (maxNumber: int) =
        let numberSum = arithProgressionSum 1 maxNumber maxNumber
        let squareSum = seq {1 .. maxNumber} |> Seq.map (fun number -> number * number) |> Seq.sum
        numberSum * numberSum - squareSum

    [<TestCase(10, 2640, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 25164150, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
