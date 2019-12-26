namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
// In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
// Find the smallest cube for which exactly five permutations of its digits are cube.

[<TestFixture>]
type Problem062() =

    let rec processNumber (number: int64) (rangeSup: int64) (storage: IDictionary<int list, ResizeArray<int64>>) (permutationsCount: int) =
        let cube = number * number * number
        match cube with
        | _ when cube > rangeSup ->
            match storage |> Seq.filter (fun kvPair -> kvPair.Value.Count >= permutationsCount) |> Seq.map (fun kvPair -> kvPair.Value) |> Seq.toList with
            | [] -> processNumber number (rangeSup * 10L) (Dictionary<int list, ResizeArray<int64>>()) permutationsCount
            | values -> values
        | _ ->
            let digits = cube |> NumbersDigits.GetDigits |> List.sort
            if digits |> storage.ContainsKey |> not then
                storage.[digits]<-ResizeArray<int64>()
            storage.[digits].Add(cube)
            processNumber (number + 1L) rangeSup storage permutationsCount

    let solveImpl (permutationsCount: int) =
        processNumber 1L 10L (Dictionary<int list, ResizeArray<int64>>()) permutationsCount |> Seq.map (fun numbers -> numbers |> Seq.min) |> Seq.min

    [<TestCase(3, 41063625L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(5, 127035954683L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(permutationsCount: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, permutationsCount)