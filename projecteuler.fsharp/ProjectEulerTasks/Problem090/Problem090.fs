namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem090() =

    let combinations = [(0, 1); (0, 4); (0, 9); (1, 6); (2, 5); (3, 6); (4, 9); (6, 4); (8, 1)]

    let checkDigit (digit: int) (dice: int) =
        match digit with
        | 6 | 9 -> (dice &&& (1 <<< 6) > 0) || (dice &&& (1 <<< 9) > 0)
        | _ -> dice &&& (1 <<< digit) > 0

    let checkCombination (diceA: int) (diceB: int) ((digit1, digit2): int * int) =
        match digit1 with
        | _ when (diceA |> checkDigit digit1) && (diceB |> checkDigit digit1) -> (diceA |> checkDigit digit2) || (diceB |> checkDigit digit2)
        | _ when (diceA |> checkDigit digit1) -> diceB |> checkDigit digit2
        | _ when (diceB |> checkDigit digit1) -> diceA |> checkDigit digit2
        | _ -> false

    let checkCombinations (diceA: int) (diceB: int) = combinations |> Seq.exists (fun combination -> combination |> checkCombination diceA diceB |> not) |> not

    let createDice (d1: int) (d2: int) (d3: int) (d4: int) (d5: int) (d6: int) =
        let mutable dice = 0
        dice <- dice ||| (1 <<< d1)
        dice <- dice ||| (1 <<< d2)
        dice <- dice ||| (1 <<< d3)
        dice <- dice ||| (1 <<< d4)
        dice <- dice ||| (1 <<< d5)
        dice <- dice ||| (1 <<< d6)
        dice

    let completeSearchSolveImpl () =
        // one dice: count of variants = C(6, 10) = 210; two dices: count of variants = 210 * 210 = 44100
        let dices = new System.Collections.Generic.HashSet<int * int>()
        let diceGenerator = seq {for d1 in {0 .. 4} do for d2 in {d1 + 1 .. 5} do for d3 in {d2 + 1 .. 6} do for d4 in {d3 + 1 .. 7} do for d5 in {d4 + 1 .. 8} do for d6 in {d5 + 1 .. 9} do yield createDice d1 d2 d3 d4 d5 d6}
        for diceA in diceGenerator do
            for diceB in diceGenerator do
                if checkCombinations diceA diceB then
                    (diceA, diceB) |> dices.Add |> ignore
        dices.Count / 2

    [<TestCase(1217, TimeThresholds.HardTimeLimit)>]
    member public this.CompleteSearchSolve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, completeSearchSolveImpl)
