namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// The sum of the squares of the first ten natural numbers is, 1^2 + 2^2 + ... + 10^2 = 385
// The square of the sum of the first ten natural numbers is, (1 + 2 + ... + 10)^2 = 55^2 = 3025
// Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.
// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

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
