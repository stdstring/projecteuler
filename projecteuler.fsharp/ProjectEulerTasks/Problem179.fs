namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same number of positive divisors. For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.

[<TestFixture>]
type Problem179() =

    let solveImpl (maxNumber: int) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        let mutable count = 0
        let mutable lastSigma0 = 2 |> sieve.CalcSigma0
        for number in seq {3 .. maxNumber} do
            let currentSigma0 = number |> sieve.CalcSigma0
            if currentSigma0 = lastSigma0 then
                count<-count+1
            lastSigma0 <- currentSigma0
        count

    [<TestCase(10000000, 986262, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(minValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, minValue)