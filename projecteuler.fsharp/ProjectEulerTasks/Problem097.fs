namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne prime of the form 2^6972593-1; it contains exactly 2,098,960 digits.
// Subsequently other Mersenne primes, of the form 2^p-1, have been found which contain more digits.
// However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433*2^7830457+1.
// Find the last ten digits of this prime number.

[<TestFixture>]
type Problem097() =

    let solveImpl (digitsCount: int) =
        let bound = pown 10L digitsCount
        let powerValue = seq { 1 .. 7830457 } |> Seq.fold (fun product _ -> product * 2L % bound) 1L
        let result = (28433L * powerValue + 1L) % bound |> string
        [String.replicate (digitsCount - result.Length) "0"; result] |> String.concat ""

    [<TestCase(10, "8739992577", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(digitsCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, digitsCount)