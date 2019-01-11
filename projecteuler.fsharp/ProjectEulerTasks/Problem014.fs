namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The following iterative sequence is defined for the set of positive integers:
// n -> n / 2 (n is even)
// n -> 3 * n + 1 (n is odd)
// Using the rule above and starting with 13, we generate the following sequence: 13  40  20  10  5  16  8  4  2  1
// It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
// Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
// Which starting number, under one million, produces the longest chain?
// NOTE: Once the chain starts the terms are allowed to go above one million.

module Problem014Impl =

    type ChainResult = {Current: int64; ChainSize: int; Chain: int64 list}

open Problem014Impl

[<TestFixture>]
type Problem014() =

    let generateNextStep (current: int64) =
        match current % 2L with
        | 0L -> current / 2L
        | 1L -> 3L * current + 1L
        | _ -> failwith "Unexpected branch of match expression"

    let saveChain (storage: SafeStorage<int>) (result: ChainResult) =
        result.Chain |> List.iteri (fun index number -> storage.SetValue(number, result.ChainSize + index + 1))

    let rec processChain (storage: SafeStorage<int>) (chain: int64 list) (current: int64) =
        match current with
        | 1L -> {ChainResult.Current = current; ChainResult.ChainSize = 0; ChainResult.Chain = chain}
        | _ ->
            match current |> storage.GetValue with
            | value when value = storage.DefaultValue -> current |> generateNextStep |> processChain storage (current :: chain)
            | value -> {ChainResult.Current = current; ChainResult.ChainSize = value; ChainResult.Chain = chain}

    let addNumber (number: int) (storage: SafeStorage<int>) =
        number |> int64 |> processChain storage [] |> saveChain storage

    let findLongestChain (storage: SafeStorage<int>) =
        seq { storage.MinNumber .. storage.MaxNumber } |> Seq.map (fun number -> number, storage.GetValue(number)) |> Seq.maxBy (fun (_, length) -> length) |> fst

    let solveImpl (maxNumber: int) =
        let storage = SafeStorage(2, maxNumber, 0)
        seq { storage.MinNumber .. storage.MaxNumber } |> Seq.iter (fun number -> addNumber number storage)
        storage |> findLongestChain

    [<TestCase(1000000, 837799, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)