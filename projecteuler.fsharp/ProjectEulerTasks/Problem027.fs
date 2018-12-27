namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// Euler discovered the remarkable quadratic formula: n^2 + n + 41
// It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.
// However, when n = 40, 40^2 + 40 + 41 = 40 * (40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.
// The incredible formula  n^2 - 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79.
// The product of the coefficients, -79 and 1601, is -126479.
// Considering quadratics of the form: n^2 + an + b, where |a| < 1000 and |b| < 1000 where |n| is the modulus/absolute value of n, e.g. |11| = 11 and |-4| = 4
// Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

module Problem027Impl =
    type SequenceResult = {A: int; B: int; PrimesCount: int}

open Problem027Impl

[<TestFixture>]
type Problem027() =

    let calcValue (number: int) (aValue: int) (bValue: int) = number * number + aValue * number + bValue

    let processSequence (aValue: int) (bValue: int) (primesSet: ISet<int>) =
        let primesCount = seq {0 .. bValue - 1} |> Seq.takeWhile (fun number -> primesSet.Contains(calcValue number aValue bValue)) |> Seq.length
        {SequenceResult.A = aValue; SequenceResult.B = bValue; SequenceResult.PrimesCount = primesCount}

    let processACoeff (aMax: int) (bValue: int) (primesSet: ISet<int>) =
        seq {-bValue .. 2 .. aMax} |> Seq.map (fun aValue -> primesSet |> processSequence aValue bValue) |> Seq.maxBy (fun result -> result.PrimesCount)

    let solveImpl (aMax: int) (bMax: int) =
        //  Notes to solution:
        // 1) N(n = 0) = b > 0 => b > 0 and b is prime number. Easy to show that b > 2 => b is odd number (according to 3)
        // 2) N(n = 1) = 1 + a + b > 0 => a > -(b + 1), a - is odd number (due to b + 1 is even number), a <> 0
        // 3) When n = b, N(n) = b^2 + a*b + b = b * (b + a + 1) - is not prime number => n in (0, Nmax], Nmax = max(b)^2 + max(a) * max(b) + max(b)
        let nMax = calcValue bMax aMax bMax
        let sieveBuilder = EratosSieveBuilder()
        let primes = sieveBuilder.CreateSieve(nMax).ToSeq() |> Seq.toList
        let primesSet = HashSet<int>(primes) :> ISet<int>
        let 2 :: bValues = primes |> List.takeWhile (fun prime -> prime < bMax)
        let result = bValues |> Seq.map (fun bValue -> primesSet |> processACoeff aMax bValue) |> Seq.maxBy (fun result -> result.PrimesCount)
        result.A * result.B

    [<TestCase(1000, 1000, -59231, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(aMax: int, bMax: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl aMax bMax)