namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem078() =

    let calcGeneralizedPentagonalNumber (k: int) = k * (3 * k - 1) / 2

    // TODO (std_string) : move into common libs
    let calcP (storage: ResizeArray<int>) (divider: int) (n: int) =
        let rec calcPImpl (k: int) (result: int) =
            let generalizedPentagonalNumber = k |> calcGeneralizedPentagonalNumber
            match generalizedPentagonalNumber with
            | _ when generalizedPentagonalNumber > n -> result
            | _ ->
                let sign  = k - 1 |> pown (-1)
                let newResult = (result + sign * storage.[n - generalizedPentagonalNumber]) % divider
                match k with
                | _ when k > 0 -> newResult |> calcPImpl (-k)
                | _ when k < 0 -> newResult |> calcPImpl (-k + 1)
                | _ -> failwith "Unexpected branch of match expression"
        calcPImpl 1 0

    let rec processNumber (storage: ResizeArray<int>) (divider: int) (number: int) =
        match number |> calcP storage divider with
        | 0 -> number
        | value ->
            value |> storage.Add
            number + 1 |> processNumber storage divider

    // implementation details: https://en.wikipedia.org/wiki/Partition_function_(number_theory), https://en.wikipedia.org/wiki/Pentagonal_number_theorem
    let solveImpl (divider: int) =
        let storage = ResizeArray<int>()
        1 |> storage.Add
        1 |> processNumber storage divider

    [<TestCase(1000000, 55374, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(divider: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, divider)
