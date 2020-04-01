﻿namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

// The binomial coefficients (n, k) can be arranged in triangular form, Pascal's triangle, like this:
// 1
// 1 1
// 1 2 1
// 1 3 3 1
// 1 4 6 4 1
// 1 5 10 10 5 1
// 1 6 15 20 15 6 1
// 1 7 21 35 35 21 7 1
// .........
// It can be seen that the first eight rows of Pascal's triangle contain twelve distinct numbers: 1, 2, 3, 4, 5, 6, 7, 10, 15, 20, 21 and 35.
// A positive integer n is called squarefree if no square of a prime divides n. Of the twelve distinct numbers in the first eight rows of Pascal's triangle, all except 4 and 20 are squarefree.
// The sum of the distinct squarefree numbers in the first eight rows is 105.
// Find the sum of the distinct squarefree numbers in the first 51 rows of Pascal's triangle.

module Problem203Impl =
    type FactorsData = {Factors: int[]}

open Problem203Impl

[<TestFixture>]
type Problem203() =

    [<Literal>]
    let MinNumber = 2

    let mergeFactors (sourceFactors: FactorsData) (destFactors: int[]) =
        sourceFactors.Factors |> Array.iteri (fun index value -> destFactors.[index] <- destFactors.[index] + value)
        destFactors

    let strikeOutFactors (sourceFactors: int[]) (destFactors: int[]) =
        for index in seq {0 .. destFactors.Length - 1} do
            if destFactors.[index] < sourceFactors.[index] then
                raise (System.InvalidOperationException())
            destFactors.[index] <- destFactors.[index] - sourceFactors.[index]
        destFactors

    let isSquarefreeNumbers (factors: int[]) =
        factors |> Array.exists (fun value -> value > 1) |> not

    let constructNumber (factors: int[]) =
        seq {0 .. factors.Length - 1} |> Seq.filter (fun index -> factors.[index] = 1) |> Seq.fold (fun result index -> let factor = MinNumber + index |> int64 in factor * result) 1L

    let createNumberFactors (dataSize: int) (sieve: EratosSieveWithSmallestPrimeFactors) (number: int) =
        let rec fillData (factors: int[]) (number: int) =
            let factor = sieve.[number]
            factors.[factor - MinNumber] <- factors.[factor - MinNumber] + 1
            match number = factor with
            | false -> number / factor |> fillData factors
            | true -> ()
        let factors = dataSize |> Array.zeroCreate
        number |> fillData factors
        {FactorsData.Factors = factors}

    let createFactorialFactors (dataSize: int) (numbersFactors: FactorsData []) (number: int) =
        let factors = seq {2 .. number} |> Seq.fold (fun dest number -> dest |> mergeFactors numbersFactors.[number - MinNumber]) (dataSize |> Array.zeroCreate)
        {FactorsData.Factors = factors}

    let processBinominalCoefficient (dataSize: int) (dest: ISet<int64>) (numbersFactors: FactorsData []) (factorialsFactors: FactorsData []) (row: int) (column: int) =
        let factorialFactors = if column = 1 then dataSize |> Array.zeroCreate else factorialsFactors.[column - MinNumber].Factors
        let destFactors = seq {row - column + 1 .. row} |> Seq.fold (fun dest number -> dest |> mergeFactors numbersFactors.[number - MinNumber]) (dataSize |> Array.zeroCreate) |> strikeOutFactors factorialFactors
        match destFactors |> isSquarefreeNumbers with
        | false -> ()
        | true -> destFactors |> constructNumber |> dest.Add |> ignore

    let processRow (dataSize: int) (dest: ISet<int64>) (numbersFactors: FactorsData []) (factorialsFactors: FactorsData []) (row: int) =
        seq {1 .. row / 2} |> Seq.iter (fun column -> column |> processBinominalCoefficient dataSize dest numbersFactors factorialsFactors row)

    let solveImpl (rowCount: int) =
        let maxRowNumber = rowCount - 1
        let dataSize = maxRowNumber - MinNumber + 1
        let sieve = maxRowNumber |> EratosSieveWithSmallestPrimeFactors.Create
        let numbersFactors = seq {MinNumber .. maxRowNumber} |> Seq.map (fun number -> number |> createNumberFactors dataSize sieve) |> Seq.toArray
        let factoralMax = maxRowNumber / 2
        let factorialsFactors = seq {MinNumber .. factoralMax} |> Seq.map (fun number -> number |> createFactorialFactors dataSize numbersFactors) |> Seq.toArray
        // data for rows with numbers 0 - 3
        let dest = [1L; 2L; 3L] |> HashSet<int64> :> ISet<int64>
        seq {4 .. maxRowNumber} |> Seq.iter (fun row -> row |> processRow dataSize dest numbersFactors factorialsFactors)
        dest |> Seq.sum

    [<TestCase(8, 105L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(51, 34029210557338L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(rowCount: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, rowCount)
