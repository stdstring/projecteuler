namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

module Problem092Impl =

    type ChainResult = {Current: int; StopValue: int; Chain: int list}

open Problem092Impl

[<TestFixture>]
type Problem092() =

    let digitSquares = seq { 0 .. 9 } |> Seq.map (fun digit -> digit * digit) |> Seq.toArray

    let generateNextStep (current: int) =
        current |> NumbersDigits.GetDigits |> Seq.map (fun digit -> digitSquares.[digit]) |> Seq.sum

    let saveChain (storage: SafeArray<int>) (result: ChainResult) =
        result.Chain |> List.iter (fun number -> storage.SetValue(number, result.StopValue))

    let rec processChain (storage: SafeArray<int>) (chain: int list) (current: int) =
        match current |> storage.GetValue with
        | value when value = storage.DefaultValue -> current |> generateNextStep |> processChain storage (current :: chain)
        | value -> {ChainResult.Current = current; ChainResult.StopValue = value; ChainResult.Chain = chain}

    let addNumber (number: int) (storage: SafeArray<int>) =
        number |> processChain storage [] |> saveChain storage

    let solveImpl (maxNumber: int) =
        let expectedStopValue = 89
        let storage = SafeArray(1, maxNumber, 0)
        // known values
        [1; 89] |> Seq.iter (fun value -> storage.SetValue(value, value))
        seq { storage.MinNumber .. storage.MaxNumber } |> Seq.iter (fun number -> addNumber number storage)
        storage.Storage |> Seq.filter (fun value -> value = expectedStopValue) |> Seq.length

    [<TestCase(100, 80, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 857, TimeThresholds.HardTimeLimit)>]
    [<TestCase(9999999, 8581146, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)