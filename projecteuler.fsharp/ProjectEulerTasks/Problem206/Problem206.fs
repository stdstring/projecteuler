namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem206() =

    let infValue = 101010103
    let supValue = 138902661

    let checkNumber (number: int64) =
        match number % 1000L with
        | 809L | 829L | 849L | 869L | 889L ->
            match number / 10000L |> NumbersDigits.GetDigits with
            | [1; _; 2; _; 3; _; 4; _; 5; _; 6; _; 7] -> true
            | _ -> false
        | _ -> false

    // delta = 4, 6
    let rec processNumber (number: int) (delta: int) =
        match number with
        | _ when number > supValue -> failwith "Logic error"
        | _ ->
            let squareValue = (number |> int64) * (number |> int64)
            match squareValue |> checkNumber with
            | true -> number
            | false -> (10 - delta) |> processNumber (number + delta)

    let solveImpl () =
        // notes:
        // Square = Number * Number = 1_2_3_4_5_6_7_8_9_0
        // Min(Square) = 1020304050607080900
        // Max(Square) = 1929394959697989990
        // Min(Number) = 1010101010,...
        // Max(Number) = 1389026623,...
        // Square = Number * Number = 1_2_3_4_5_6_7_8_900 due to properties of the square numbers
        // Let Number = 10 * Number' => Number * Number = (Number' * Number') * 100 => Square = Square' * 100
        // Square' = 1_2_3_4_5_6_7_8_9
        // Min(Number') = 101010101,...
        // Max(Number') = 138902662,...
        // Square' = 1_2_3_4_5_6_7_809, 1_2_3_4_5_6_7_829, 1_2_3_4_5_6_7_849, 1_2_3_4_5_6_7_869, 1_2_3_4_5_6_7_889  due to properties of the square numbers
        // Number' = XXXXXXXX3, XXXXXXXX7 due to Square' finished on digit 9
        processNumber infValue 4 |> (*) 10


    [<TestCase(1389019170, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)