namespace ProjectEulerTasks

open CommonLib
open CommonLib.DynamicProgramming
open NUnit.Framework
open ProjectEulerTasks.Utils

// It is possible to write ten as the sum of primes in exactly five different ways:
// 7 + 3
// 5 + 5
// 5 + 3 + 2
// 3 + 3 + 2 + 2
// 2 + 2 + 2 + 2 + 2
// What is the first value which can be written as the sum of primes in over five thousand different ways?

[<TestFixture>]
type Problem077() =

    [<Literal>]
    let MaxSieveNumber = 1000

    let solveImpl (minValue: int64) =
        let sieve = MaxSieveNumber |> EratosSieve.Create
        let primes = sieve.ToSeq() |> Seq.toArray
        let partitions = SumPartitions<int>.CreateInt64(MaxSieveNumber, primes)
        seq {1 .. MaxSieveNumber} |> Seq.skipWhile (fun number -> partitions.GetPartitionCount(number) <= minValue) |> Seq.head

    [<TestCase(4, 10, TimeThresholds.HardTimeLimit)>]
    [<TestCase(5000, 71, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(minValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, minValue |> int64)
