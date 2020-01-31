namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils
open System

// Let N be a positive integer and let N be split into k equal parts, r = N / k, so that N = r + r + ... + r.
// Let P be the product of these parts, P = r * r * ... * r = r^k.
// For example, if 11 is split into five equal parts, 11 = 2.2 + 2.2 + 2.2 + 2.2 + 2.2, then P = 2.2^5 = 51.53632.
// Let M(N) = Pmax for a given value of N.
// It turns out that the maximum for N = 11 is found by splitting eleven into four equal parts which leads to Pmax = (11/4)^4; that is, M(11) = 14641/256 = 57.19140625, which is a terminating decimal.
// However, for N = 8 the maximum is achieved by splitting it into three equal parts, so M(8) = 512/27, which is a non-terminating decimal.
// Let D(N) = N if M(N) is a non-terminating decimal and D(N) = -N if M(N) is a terminating decimal.
// For example, SUM(D(N)) for 5 <= N <= 100 is 2438.
// Find SUM(D(N)) for 5 <= N <= 10000.

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