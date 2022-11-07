namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem088Impl =
    type FactorsData = {Factors: int[]; Number: int; K: int}

open Problem088Impl

[<TestFixture>]
type Problem088() =

    let rec putFactorOnPlace (factors: int[]) (index: int) =
        match index with
        | _ when index = factors.Length - 1 -> ()
        | _ when factors.[index] <= factors.[index + 1] -> ()
        | _ ->
            let value = factors.[index]
            factors.[index] <- factors.[index + 1]
            factors.[index + 1] <- value
            index + 1 |> putFactorOnPlace factors

    // TODO (std_string) : think about more smart approach
    let generateFactors (number: int) (factor: int) (factorData: FactorsData) =
        let rec generateImpl (factorsTemplate: int[]) (index: int) (dest: FactorsData list) =
            match index = factorsTemplate.Length with
            | true -> dest
            | false ->
                let factors = factorsTemplate |> Array.copy
                factors.[index] <- factors.[index] * factor
                let factorsSum = factors |> Seq.sum
                index |> putFactorOnPlace factors
                {FactorsData.Factors = factors; FactorsData.Number = number; FactorsData.K = factors.Length + (number - factorsSum)} :: dest |> generateImpl factorsTemplate (index + 1)
        let initFactors = factorData.Factors.Length + 1 |> Array.zeroCreate
        initFactors.[0] <- factor
        System.Array.Copy(factorData.Factors, 0, initFactors, 1, factorData.Factors.Length)
        0 |> putFactorOnPlace initFactors
        let initFactorsSum = initFactors |> Seq.sum
        [{FactorsData.Factors = initFactors; FactorsData.Number = number; FactorsData.K = initFactors.Length + (number - initFactorsSum)}] |> generateImpl factorData.Factors 0

    // TODO (std_string) : think about more smart approach
    let generateAllFactors (number: int) (factor: int) (numberFactorsData: FactorsData list) =
        let initFactors = [|factor; number / factor|]
        let initFactorsSum = factor + (number / factor)
        let initFactorsData = {FactorsData.Factors = initFactors; FactorsData.Number = number; FactorsData.K = initFactors.Length + (number - initFactorsSum)}
        initFactorsData :: (numberFactorsData |> Seq.map (fun factorData -> factorData |> generateFactors number factor) |> Seq.concat |> Seq.toList) |> List.distinct

    let solveImpl (maxK: int) =
        // maxNumber = 2 * x => maxK = 2 + (2 * x - x - 2) = x => maxNumber = 2 * maxK
        let maxNumber = 2 * maxK
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        let numberFactorsData = Array.create (maxNumber + 1) []
        for number in seq {2 .. maxNumber} do
            if number |> sieve.IsPrime then
                numberFactorsData.[number] <- []
            else
                let factor = sieve.[number]
                numberFactorsData.[number] <- numberFactorsData.[number / factor] |> generateAllFactors number factor
        let resultData = numberFactorsData |> Seq.concat |> Seq.filter (fun data -> data.K > 1 && data.K <= maxK) |> Seq.sortBy (fun data -> data.K) |> Seq.distinctBy (fun data -> data.K) |> Seq.toList
        let minProductSumNumbers = new System.Collections.Generic.HashSet<int>() :> System.Collections.Generic.ISet<int>
        resultData |> Seq.iter (fun data -> data.Number |> minProductSumNumbers.Add |> ignore)
        minProductSumNumbers |> Seq.sum

    [<TestCase(6, 30, TimeThresholds.HardTimeLimit)>]
    [<TestCase(12, 61, TimeThresholds.HardTimeLimit)>]
    [<TestCase(12000, 7587457, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxK: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxK)
