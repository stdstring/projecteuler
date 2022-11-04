namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

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
        let primes = EratosSieve.Create(nMax).ToSeq() |> Seq.toList
        let primesSet = HashSet<int>(primes) :> ISet<int>
        let bValues = primes |> List.skip 1 |> List.takeWhile (fun prime -> prime < bMax)
        let result = bValues |> Seq.map (fun bValue -> primesSet |> processACoeff aMax bValue) |> Seq.maxBy (fun result -> result.PrimesCount)
        result.A * result.B

    [<TestCase(1000, 1000, -59231, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(aMax: int, bMax: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, aMax, bMax)
