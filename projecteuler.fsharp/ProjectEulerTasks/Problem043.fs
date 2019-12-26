namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
// Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
// d2d3d4=406 is divisible by 2
// d3d4d5=063 is divisible by 3
// d4d5d6=635 is divisible by 5
// d5d6d7=357 is divisible by 7
// d6d7d8=572 is divisible by 11
// d7d8d9=728 is divisible by 13
// d8d9d10=289 is divisible by 17
// Find the sum of all 0 to 9 pandigital numbers with this property.

[<TestFixture>]
type Problem043() =

    let allDigits = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

    let selectDigits (digits: List<int>) = List.except digits allDigits |> List.map (fun digit -> digit :: digits)

    let choosed8d9d10 (number: int) =
        match number |> NumbersDigits.GetDigits with
        | [d9; d10] when d9 = d10 -> None
        | [d9; d10] -> Some [0; d9; d10]
        | [d8; d9; d10] when (d8 = d9) || (d9 = d10) || (d10 = d8) -> None
        | [d8; d9; d10] -> Some [d8; d9; d10]
        | _ -> failwith "Unexpected branch of match expression"

    // divisible by 13
    let choosed7d8d9 (digits: List<int>) =
        match digits with
        | [d7; d8; d9; _] when (100 * d7 + 10 * d8 + d9) % 13 = 0 -> Some digits
        | _ -> None

    // divisible by 11
    let choosed6d7d8 (digits: List<int>) =
        match digits with
        | [d6; d7; d8; _; _] when (100 * d6 + 10 * d7 + d8) % 11 = 0 -> Some digits
        | _ -> None

    // divisible by 7
    let choosed5d6d7 (digits: List<int>) =
        match digits with
        | [d5; d6; d7; _; _; _] when (100 * d5 + 10 * d6 + d7) % 7 = 0 -> Some digits
        | _ -> None

    // divisible by 5
    let choosed4d5d6 (digits: List<int>) =
        match digits with
        | [d4; d5; d6; _; _; _; _] when (100 * d4 + 10 * d5 + d6) % 5 = 0 -> Some digits
        | _ -> None

    // divisible by 3
    let choosed3d4d5 (digits: List<int>) =
        match digits with
        | [d3; d4; d5; _; _; _; _; _] when (100 * d3 + 10 * d4 + d5) % 3 = 0 -> Some digits
        | _ -> None

    // divisible by 2
    let choosed2d3d4 (digits: List<int>) =
        match digits with
        | [d2; d3; d4; _; _; _; _; _; _] when (100 * d2 + 10 * d3 + d4) % 2 = 0 -> Some digits
        | _ -> None

    let chooseStep (chooseFun: 'TInput -> List<int> option) (source: seq<'TInput>) = source |> Seq.choose chooseFun |> Seq.map selectDigits |> Seq.concat

    let solveImpl () =
        seq {17 .. 17 .. 999} |> chooseStep choosed8d9d10 |>
                                 chooseStep choosed7d8d9 |>
                                 chooseStep choosed6d7d8 |>
                                 chooseStep choosed5d6d7 |>
                                 chooseStep choosed4d5d6 |>
                                 chooseStep choosed3d4d5 |>
                                 chooseStep choosed2d3d4 |>
                                 Seq.map (fun digits -> NumbersDigits.GetNumber(digits) |> int64 ) |> Seq.sum

    [<TestCase(16695334890L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)