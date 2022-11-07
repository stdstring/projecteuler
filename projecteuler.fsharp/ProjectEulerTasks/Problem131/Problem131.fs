namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

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