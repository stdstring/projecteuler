namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:
// 1! + 4! + 5! = 1 + 24 + 120 = 145
// Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169;
// it turns out that there are only three such loops that exist:
// 169 -> 363601 -> 1454 -> 169
// 871 -> 45361 -> 871
// 872 -> 45362 -> 872
// It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,
// 69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
// 78 -> 45360 -> 871 -> 45361 (-> 871)
// 540 -> 145 (-> 145)
// Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.
// How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?

module Problem074Impl =

    type ChainResult =
    | LoopDetected of current: int * loopSize: int * chain: int list
    | SavedValueDetected of current: int * loopSize: int * chain: int list

open Problem074Impl

[<TestFixture>]
type Problem074() =

    let digitFactorials = seq { 0 .. 9 } |> Seq.map (fun digit -> digit |> Numbers.CalcFactorial |> int) |> Seq.toArray

    let generateNextStep (current: int) =
        current |> NumbersDigits.GetDigits |> Seq.map (fun digit -> digitFactorials.[digit]) |> Seq.sum

    let getChainLoop (current: int) (chain: int list) =
        match chain |> List.tryFindIndex (fun number -> number = current) with
        | Some index -> index + 1 |> Some
        | None -> None

    let saveChain (storage: SafeStorage<int>) (result: ChainResult) =
        match result with
        | ChainResult.LoopDetected (current, loopSize, chain) ->
            chain |> Seq.take loopSize |> Seq.iter (fun number -> storage.SetValue(number, loopSize))
            chain |> Seq.skip loopSize |> Seq.iteri (fun index number -> storage.SetValue(number, loopSize + index + 1))
        | ChainResult.SavedValueDetected (current, loopSize, chain) ->
            chain |> Seq.iteri (fun index number -> storage.SetValue(number, loopSize + index + 1))

    let rec processChain (storage: SafeStorage<int>) (chain: int list) (current: int) =
        match current |> storage.GetValue with
        | value when value = storage.DefaultValue ->
            match getChainLoop current chain with
            | Some loopSize -> ChainResult.LoopDetected (current, loopSize, chain)
            | None -> current |> generateNextStep |> processChain storage (current :: chain)
        | value -> ChainResult.SavedValueDetected (current, value, chain)

    let addNumber (number: int) (storage: SafeStorage<int>) =
        number |> processChain storage [] |> saveChain storage

    let solveImpl (maxNumber: int) (expectedChainSize: int) =
        let storage = SafeStorage(1, maxNumber, 0)
        seq { storage.MinNumber .. storage.MaxNumber } |> Seq.iter (fun number -> addNumber number storage)
        storage.Storage |> Seq.filter (fun value -> value = expectedChainSize) |> Seq.length

    [<TestCase(999999, 60, 402, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedChainSize: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, expectedChainSize)