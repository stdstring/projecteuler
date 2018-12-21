namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17. Find the sum of all the primes below two million.

[<TestFixture>]
type Problem010() =

    let solveImpl (maxNumber: int) =
        let sieveBuilder = EratosSieveBuilder()
        sieveBuilder.CreateSieve(maxNumber).ToSeq() |> Seq.map int64 |> Seq.sum

    [<TestCase(10, 17L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1999999, 142913828922L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl maxNumber)
