namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

module Problem074Impl =

    type ChainResult =
    | LoopDetected of current: int * loopSize: int * chain: int list
    | SavedValueDetected of current: int * loopSize: int * chain: int list

open Problem074Impl

[<TestFixture>]
type Problem074() =

    let digitFactorials = seq {0 .. 9} |> Seq.map (fun digit -> digit |> Numbers.CalcFactorial |> int) |> Seq.toArray

    let generateNextStep (current: int) =
        current |> NumbersDigits.GetDigits |> Seq.map (fun digit -> digitFactorials.[digit]) |> Seq.sum

    let getChainLoop (current: int) (chain: int list) =
        match chain |> List.tryFindIndex (fun number -> number = current) with
        | Some index -> index + 1 |> Some
        | None -> None

    let saveChain (storage: SafeArray<int>) (result: ChainResult) =
        match result with
        | ChainResult.LoopDetected (current, loopSize, chain) ->
            chain |> Seq.take loopSize |> Seq.iter (fun number -> storage.SetValue(number, loopSize))
            chain |> Seq.skip loopSize |> Seq.iteri (fun index number -> storage.SetValue(number, loopSize + index + 1))
        | ChainResult.SavedValueDetected (current, loopSize, chain) ->
            chain |> Seq.iteri (fun index number -> storage.SetValue(number, loopSize + index + 1))

    let rec processChain (storage: SafeArray<int>) (chain: int list) (current: int) =
        match current |> storage.GetValue with
        | value when value = storage.DefaultValue ->
            match getChainLoop current chain with
            | Some loopSize -> ChainResult.LoopDetected (current, loopSize, chain)
            | None -> current |> generateNextStep |> processChain storage (current :: chain)
        | value -> ChainResult.SavedValueDetected (current, value, chain)

    let addNumber (number: int) (storage: SafeArray<int>) =
        number |> processChain storage [] |> saveChain storage

    let solveImpl (maxNumber: int) (expectedChainSize: int) =
        let storage = SafeArray(1, maxNumber, 0)
        seq { storage.MinNumber .. storage.MaxNumber } |> Seq.iter (fun number -> addNumber number storage)
        storage.Storage |> Seq.filter (fun value -> value = expectedChainSize) |> Seq.length

    [<TestCase(999999, 60, 402, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedChainSize: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, expectedChainSize)