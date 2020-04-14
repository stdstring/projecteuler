namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// The largest integer <= 100 that is only divisible by both the primes 2 and 3 is 96, as 96=32*3=2^5*3.
// For two distinct primes p and q let M(p,q,N) be the largest positive integer <= N only divisible by both p and q and M(p,q,N)=0 if such a positive integer does not exist.
// E.g. M(2,3,100)=96. M(3,5,100)=75 and not 90 because 90 is divisible by 2 ,3 and 5.
// Also M(2,73,100)=0 because there does not exist a positive integer <= 100 that is divisible by both 2 and 73.
// Let S(N) be the sum of all distinct M(p,q,N). S(100)=2262.
// Find S(10 000 000).

[<TestFixture>]
type Problem347() =

    let rec tryExtractPrimeFactorsPair (sieve: EratosSieveWithSmallestPrimeFactors) (number: int) (p: int) (q: int) =
        match number with
        | 1 when q = 0 -> None
        | 1 -> (p, q) |> Some
        | _ when p = 0 ->
            let factor = sieve.[number]
            tryExtractPrimeFactorsPair sieve (number / factor) factor 0
        | _ when q = 0 && p = sieve.[number] -> tryExtractPrimeFactorsPair sieve (number / p) p 0
        | _ when q = 0 && p <> sieve.[number] ->
            let factor = sieve.[number]
            tryExtractPrimeFactorsPair sieve (number / factor) p factor
        | _ when q <> 0 && q = sieve.[number] -> tryExtractPrimeFactorsPair sieve (number / q) p q
        | _ when q <> 0 && q <> sieve.[number] -> None
        | _ -> failwith "Unexpected branch of match expression"

    let calcMValue (sieve: EratosSieveWithSmallestPrimeFactors) (used: bool[]) (number: int) =
         match tryExtractPrimeFactorsPair sieve number 0 0 with
         | None -> 0L
         | Some (p, q) ->
            let product = p * q
            match used.[product] with
            | true -> 0L
            | false ->
                used.[product] <- true
                number |> int64

    let solveImpl (maxNumber: int) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        let used = maxNumber + 1 |> Array.zeroCreate
        seq {maxNumber .. - 1 .. 2} |> Seq.map (fun number -> number |> calcMValue sieve used) |> Seq.sum

    [<TestCase(100, 2262L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10000000, 11109800204052L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
