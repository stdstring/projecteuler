namespace ProjectEulerTasks

open CommonLib.DynamicProgramming
open NUnit.Framework
open ProjectEulerTasks.Utils

// It is possible to write five as a sum in exactly six different ways:
// 4 + 1
// 3 + 2
// 3 + 1 + 1
// 2 + 2 + 1
// 2 + 1 + 1 + 1
// 1 + 1 + 1 + 1 + 1
// How many different ways can one hundred be written as a sum of at least two positive integers?

[<TestFixture>]
type Problem076() =

    let solveViaDynamicProgrammingImpl (number: int) =
        let partitions = SumPartitions<int>.CreateInt(number, [|1 .. number - 1|])
        number |> partitions.GetPartitionCount

    let calcGeneralizedPentagonalNumber (k: int) = k * (3 * k - 1) / 2

    (*let calcP (storage: ResizeArray<int>) (n: int) =
        seq {for i in 1 .. n do yield! [i; -i]} |>
        Seq.map (fun k -> ((k - 1) |> pown (-1)), (k |> calcGeneralizedPentagonalNumber)) |>
        Seq.takeWhile (fun (_, number) -> number <= n) |>
        Seq.map (fun (sign, number) -> sign * storage.[n - number]) |>
        Seq.sum*)

    // TODO (std_string) : move into common libs
    let calcP (storage: ResizeArray<int>) (n: int) =
        let rec calcPImpl (k: int) (result: int) =
            let generalizedPentagonalNumber = k |> calcGeneralizedPentagonalNumber
            match generalizedPentagonalNumber with
            | _ when generalizedPentagonalNumber > n -> result
            | _ ->
                let sign  = k - 1 |> pown (-1)
                let newResult = result + sign * storage.[n - generalizedPentagonalNumber]
                match k with
                | _ when k > 0 -> newResult |> calcPImpl (-k)
                | _ when k < 0 -> newResult |> calcPImpl (-k + 1)
                | _ -> failwith "Unexpected branch of match expression"
        calcPImpl 1 0

    let rec processNumber (storage: ResizeArray<int>) (number: int) (n: int) =
        let p = n |> calcP storage
        match n with
        | _ when n = number -> p - 1
        | _ ->
            p |> storage.Add
            n + 1 |> processNumber storage number

    // implementation details: https://en.wikipedia.org/wiki/Partition_function_(number_theory), https://en.wikipedia.org/wiki/Pentagonal_number_theorem
    let solveViaPartitionFunctionImpl (number: int) =
        let storage = ResizeArray<int>()
        1 |> storage.Add
        1 |> processNumber storage number

    [<TestCase(5, 6, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 190569291, TimeThresholds.HardTimeLimit)>]
    member public this.SolveViaDynamicProgramming(number: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveViaDynamicProgrammingImpl, number)

    [<TestCase(5, 6, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 190569291, TimeThresholds.HardTimeLimit)>]
    member public this.SolveViaPartitionFunctionImpl(number: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveViaPartitionFunctionImpl, number)
