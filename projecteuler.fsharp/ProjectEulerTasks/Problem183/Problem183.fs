namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils
open System

[<TestFixture>]
type Problem183() =

    [<Literal>]
    let MinNumber = 5

    let rec isTerminatingDecimal (sieve: EratosSieveWithSmallestPrimeFactors) (k: int) =
        match k with
        | 1 -> true
        | _ when (sieve.[k] = 2) || (sieve.[k] = 5) -> (k / sieve.[k]) |> isTerminatingDecimal sieve
        | _ -> false

    let processNumber (sieve: EratosSieveWithSmallestPrimeFactors) (number: int) =
        let kReal = (number |> double) / Math.E
        let kBottom = kReal |> int
        let kTop = kBottom + 1
        let kBottomValue = (kBottom |> double) * log ((number |> double) / (kBottom |> double))
        let kTopValue = (kTop |> double) * log ((number |> double) / (kTop |> double))
        let k = if kBottomValue < kTopValue then kTop else kBottom
        let gcb = NumbersRelation.CalcGCD(number, k)
        let isTerminating = (k / gcb) |> isTerminatingDecimal sieve
        if isTerminating then -number else number

    let solveImpl (maxNumber: int) =
        // Description:
        // Let f = (N / x)^x = e^(x * ln(N / x))
        // df / dx = (ln(N / x) - 1) * e^(x * ln(N / x))
        // df / dx = 0 => x = N / e - max
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        seq {MinNumber .. maxNumber} |> Seq.map (fun number -> number |> processNumber sieve) |> Seq.sum

    [<TestCase(100, 2438, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10000, 48861552, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(minValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, minValue)