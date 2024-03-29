﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

[<TestFixture>]
type Problem103() =

    let rec checkRule2Impl (numbers: int[]) (count: int) =
        match count with
        | _ when count >= numbers.Length -> true
        | _ ->
            let minCurrentPortionSum = numbers |> Seq.take count |> Seq.sum
            let maxPrevPortionSum = numbers |> Seq.rev |> Seq.take (count - 1) |> Seq.sum
            match maxPrevPortionSum < minCurrentPortionSum with
            | false -> false
            | true -> (count + 1) |> checkRule2Impl numbers

    let checkRule2 (numbers: int[]) =
        checkRule2Impl numbers 2

    let rec checkRule1Impl (numbers: int[]) (currentValue: int) (size: int) (index: int) (subsets: ISet<int>[]) =
        match index with
        | _ when index = numbers.Length -> true
        | _ ->
            let nextValue = currentValue + numbers.[index]
            let nextSize = size + 1
            match nextValue |> subsets.[nextSize].Add with
            | false -> false
            | true ->
                match checkRule1Impl numbers nextValue nextSize (index + 1) subsets with
                | false -> false
                | true -> checkRule1Impl numbers currentValue size (index + 1) subsets

    let checkRule1 (numbers: int[]) =
        let subsets = numbers.Length + 1 |> Array.zeroCreate
        for index in 1 .. numbers.Length do
            subsets.[index] <- (new HashSet<int>() :> ISet<int>)
        checkRule1Impl numbers 0 0 0 subsets

    let checkRules (numbers: int[]) = (numbers |> checkRule1) && (numbers |> checkRule2)

    let calcNumberSequenceSum (start: int) (count: int) =
        // arith progression
        count * (2 * start + 1 * (count - 1)) / 2

    let generatePossibleSets (firstNumber: int) (totalSize: int) (maxSum: int) =
        let possibleSets = new ResizeArray<int[]>()
        let rec generateImpl (prefixSum: int) (currentIndex: int) (possibleSet: int[]) =
            let currentSize = currentIndex + 1
            let restMaxSum = maxSum - prefixSum
            possibleSet.[currentIndex] <- 1 + possibleSet.[currentIndex - 1]
            while (calcNumberSequenceSum possibleSet.[currentIndex] (totalSize - currentSize + 1)) <= restMaxSum do
                if currentSize = totalSize then
                    let numbers = possibleSet |> Seq.toArray
                    if numbers |> checkRules then
                        numbers |> possibleSets.Add
                else
                    possibleSet |> generateImpl (prefixSum + possibleSet.[currentIndex]) (currentIndex + 1)
                possibleSet.[currentIndex] <- possibleSet.[currentIndex] + 1
        let possibleSet = totalSize |> Array.zeroCreate
        possibleSet.[0] <- firstNumber
        possibleSet |> generateImpl firstNumber 1
        possibleSets

    let generateNearOptimumSet (prevOptimumSet: int[]) =
        let middle = prevOptimumSet.[prevOptimumSet.Length / 2]
        prevOptimumSet |> Array.map (fun element -> middle + element) |> Array.append [|middle|]

    let solveImpl (size: int) (prevOptimumSetStr: string) =
        let prevOptimumSet = prevOptimumSetStr.Split(',') |> Array.map int
        let nearOptimumSet = prevOptimumSet |> generateNearOptimumSet
        let nearOptimumSetSum = nearOptimumSet |> Seq.sum
        let bestSet = nearOptimumSetSum |> generatePossibleSets nearOptimumSet.[0] size |> Seq.minBy (fun current -> current |> Seq.sum)
        bestSet |> Seq.map string |> String.concat ""

    [<TestCase(6, "6,9,11,12,13", "111819202225", TimeThresholds.HardTimeLimit)>]
    [<TestCase(7, "11,18,19,20,22,25", "20313839404245", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(size: int, prevOptimumSetStr: string, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, size, prevOptimumSetStr)
