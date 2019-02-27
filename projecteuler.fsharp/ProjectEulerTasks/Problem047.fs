namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// The first two consecutive numbers to have two distinct prime factors are:
// 14 = 2 * 7
// 15 = 3 * 5
// The first three consecutive numbers to have three distinct prime factors are:
// 644 = 2^2 * 7 * 23
// 645 = 3 * 5 * 43
// 646 = 2 * 17 * 19.
// Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?

[<TestFixture>]
type Problem047() =

    [<Literal>]
    let MinNumber = 2

    let rec calcPrimeDividers (divider: int) (number: int) (storage: ResizeArray<Set<int>>) =
        match divider with
        | _ when divider * divider > number -> Set<int>([number])
        | 2 when number % 2 = 0 -> storage.[number / 2 - MinNumber].Add(2)
        | 2 -> storage |> calcPrimeDividers 3 number
        | divider when number % divider = 0 -> storage.[number / divider - MinNumber].Add(divider)
        | divider -> storage |> calcPrimeDividers (divider + 2) number

    let getPrimeDividers (number: int) (storage: ResizeArray<Set<int>>) =
        match number with
        | _ when (number - MinNumber) < storage.Count -> storage.[number - MinNumber]
        | _ when (number - MinNumber) = storage.Count ->
            let dividers = storage |> calcPrimeDividers 2 number
            storage.Add(dividers)
            dividers
        | _ -> failwith "Unexpected branch of match expression"

    let checkConsecutiveNumbers (startNumber: int) (count: int) (storage: ResizeArray<Set<int>>) =
        seq {startNumber .. startNumber + count - 1} |> Seq.map (fun number -> storage |> getPrimeDividers number) |> Seq.exists (fun dividers -> dividers.Count <> count) |> not

    let rec processNumber (number: int) (count: int) (storage: ResizeArray<Set<int>>) =
        match storage |> checkConsecutiveNumbers number count with
        | true -> number
        | false -> storage |> processNumber (number + 1) count

    let solveImpl (count: int) =
        ResizeArray<Set<int>>() |> processNumber MinNumber count

    [<TestCase(2, 14, TimeThresholds.HardTimeLimit)>]
    [<TestCase(3, 644, TimeThresholds.HardTimeLimit)>]
    [<TestCase(4, 134043, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(count: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, count)