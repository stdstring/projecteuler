namespace ProjectEulerTasks

open CommonLib
open CommonLib.DynamicProgramming
open NUnit.Framework
open ProjectEulerTasks.Utils

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
