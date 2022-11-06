namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

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
