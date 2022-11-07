namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem204() =

    let rec calcHammingNumbers (primes: int64[]) (start: int) (result: int64) (maxNumber: int64) =
        seq {start .. primes.Length - 1}
            |> Seq.takeWhile (fun index -> result * primes.[index] <= maxNumber)
            |> Seq.map (fun index -> 1 + calcHammingNumbers primes index (result * primes.[index]) maxNumber)
            |> Seq.sum

    let solveImpl (maxNumber: int) (hammingType: int) =
        let primes = (hammingType |> EratosSieve.Create).ToSeq() |> Seq.takeWhile (fun prime -> prime <= hammingType) |> Seq.map (fun number -> number |> int64) |> Seq.toArray
        1 + calcHammingNumbers primes 0 1L (maxNumber |> int64)

    [<TestCase(100000000, 5, 1105, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000000, 100, 2944730, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, hammingType: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, hammingType)
