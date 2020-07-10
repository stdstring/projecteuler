namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// There are some prime values, p, for which there exists a positive integer, n, such that the expression n^3 + n^2 * p is a perfect cube.
// For example, when p = 19, 8^3 + 8^2 * 19 = 123.
// What is perhaps most surprising is that for each prime with this property the value of n is unique, and there are only four such primes below one-hundred.
// How many primes below one million have this remarkable property?

[<TestFixture>]
type Problem131() =

    let calcPrimes (sieve: EratosSieve) =
        let rec calcPrimeImpl (n: int) (count: int) =
            let number =  3 * n * n + 3 * n + 1
            match number with
            | _ when number > sieve.MaxNumber -> count
            | _ when number |> sieve.IsPrime -> count + 1 |> calcPrimeImpl (n + 1)
            | _ -> count |> calcPrimeImpl (n + 1)
        calcPrimeImpl 1 0

    let solveImpl (maxNumber: int) =
        // n^3 + n^2 * p = n^2 * (n + p) => n^2 must be perfect cube and (n + p) must be perfect cube => n must be perfect cube, p must be difference between perfect cubes
        // p = a^3 - b^3 = (a - b) * (a^2 + a * b + b^2) => a - b = 1 => p must be difference between neighbour perfect cubes, i.e. p = (n + 1)^3 - n^3
        // p = (n + 1)^3 - n^3 = 3 * n^2 + 3 * n + 1, n = 1, ...
        maxNumber |> EratosSieve.Create |> calcPrimes

    [<TestCase(100, 4, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 173, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)