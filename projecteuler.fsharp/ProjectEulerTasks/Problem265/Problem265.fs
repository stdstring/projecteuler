namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem265Impl =
    type TaskConfig = {TotalSize: int; SubsequenceSize: int; SubsequenceMask: int}
    type TaskState = {KnownSubsequences: uint32; Sequence: int; SequenceSize: int}

open Problem265Impl

[<TestFixture>]
type Problem265() =

    // sequence is started from n zeros
    let rec checkSequenceTail (config: TaskConfig) (state: TaskState) (tailLeftShift: int) =
        match tailLeftShift = config.SubsequenceSize with
        | true -> true
        | false ->
            let subsequence = config.SubsequenceMask &&& (state.Sequence <<< tailLeftShift)
            let subsequenceMask = 1u <<< subsequence
            match state.KnownSubsequences &&& subsequenceMask with
            | 0u -> tailLeftShift + 1 |> checkSequenceTail config {state with KnownSubsequences = state.KnownSubsequences ||| subsequenceMask}
            | _ -> false

    // generate and check De Bruijn sequences on fly (see here https://en.wikipedia.org/wiki/De_Bruijn_sequence)
    let rec processSequence (config: TaskConfig) (state: TaskState) (result: int64) =
        match state.SequenceSize = config.TotalSize with
        | true ->
            match 1 |> checkSequenceTail config state with
            | true -> result + (state.Sequence |> int64)
            | false -> result
        | false ->
            result |> updateSequence config state 0 |> updateSequence config state 1
    and updateSequence (config: TaskConfig) (state: TaskState) (digit: int) (result: int64) =
        let updatedSequence = (state.Sequence <<< 1) + digit
        let subsequence = updatedSequence &&& config.SubsequenceMask
        let subsequenceMask = 1u <<< subsequence
        match (state.KnownSubsequences &&& subsequenceMask) with
        | 0u ->
            let updatedState = {TaskState.KnownSubsequences = (state.KnownSubsequences ||| subsequenceMask); TaskState.Sequence = updatedSequence; TaskState.SequenceSize = state.SequenceSize + 1}
            result |> processSequence config updatedState
        | _ -> result

    let solveImpl (n: int) =
        let totalSize = pown 2 n
        let mask = (pown 2 n) - 1
        // from task conditions we know, that all sequences must start from the following prefix: n zeros and 1 one
        // this prefix contains the following subsequences: 0b0...0 (n zeros), 0b0...01 (n - 1 zeros and 1 one)
        let sequence = 0b1
        let knownSubsequences = 0b11u
        let sequenceSize = n + 1
        let taskConfig = {TaskConfig.TotalSize = totalSize; TaskConfig.SubsequenceSize = n; TaskConfig.SubsequenceMask = mask}
        let initState = {TaskState.KnownSubsequences = knownSubsequences; TaskState.Sequence = sequence; TaskState.SequenceSize = sequenceSize}
        0L |> processSequence taskConfig initState

    [<TestCase(3, 52L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(5, 209110240768L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(n: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, n)
