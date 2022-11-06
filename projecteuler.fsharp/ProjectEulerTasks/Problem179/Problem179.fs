namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

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