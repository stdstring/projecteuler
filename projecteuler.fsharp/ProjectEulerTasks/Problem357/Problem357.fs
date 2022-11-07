namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem357() =

    [<Literal>]
    let StartNumber = 6

    let rec checkFactors (sieve: EratosSieve) (number: int) (factor: int) =
        match factor with
        | _ when factor * factor > number -> true
        | _ when number % factor = 0 && (number / factor) % factor = 0 -> false
        | _ when number % factor = 0 ->
            match factor + number / factor |> sieve.IsPrime with
            | true -> factor + 1 |> checkFactors sieve number
            | false -> false
        | _ -> factor + 1 |> checkFactors sieve number

    let solveImpl (maxNumber: int) =
        // Notes:
        // 1) 1 + n / 1 = n + 1 - must be prime
        // 2) if n % a = 0 and n % (a * a) = 0 then a + n / a = [ let r = n / (a * a) ] = a * (1 + r) - isn't prime
        // 3) if n is odd and a is its divider (which must be odd also) then n / a is odd also => a + n / a is even and, obviously, isn't prime
        let sieve = maxNumber + 1 |> EratosSieve.Create
        // we take into account 1 and 2 by hand and start processing from 6
        seq {StartNumber .. 4 .. maxNumber} |>
        Seq.filter (fun number -> (1 + number |> sieve.IsPrime) && (2 + number / 2 |> sieve.IsPrime) && (3 |> checkFactors sieve number)) |>
        Seq.map (fun number -> number |> int64) |>
        Seq.sum |>
        (+) (1L + 2L)

    [<TestCase(100000000, 1739023853137L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)