namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// A Hamming number is a positive number which has no prime factor larger than 5.
// So the first few Hamming numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15.
// There are 1105 Hamming numbers not exceeding 10^8.
// We will call a positive number a generalised Hamming number of type n, if it has no prime factor larger than n.
// Hence the Hamming numbers are the generalised Hamming numbers of type 5.
// How many generalised Hamming numbers of type 100 are there which don't exceed 10^9?

[<TestFixture>]
type Problem204() =

    let rec calcHammingNumbers (primes: int64[]) (start: int) (result: int64) (maxNumber: int64) =
        seq {start .. primes.Length - 1} |> Seq.takeWhile (fun index -> result * primes.[index] <= maxNumber) |> Seq.map (fun index -> 1 + calcHammingNumbers primes index (result * primes.[index]) maxNumber) |> Seq.sum

    let solveImpl (maxNumber: int) (hammingType: int) =
        let primes = (hammingType |> EratosSieve.Create).ToSeq() |> Seq.takeWhile (fun prime -> prime <= hammingType) |> Seq.map (fun number -> number |> int64) |> Seq.toArray
        1 + calcHammingNumbers primes 0 1L (maxNumber |> int64)

    [<TestCase(100000000, 5, 1105, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000000, 100, 2944730, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, hammingType: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, hammingType)
