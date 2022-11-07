namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem109() =

    [<Literal>]
    let MinBoardValue = 1

    [<Literal>]
    let MaxBoardValue = 20

    [<Literal>]
    let OuterBullValue = 25

    let generateBoard () =
        OuterBullValue |> Seq.singleton |> Seq.append (seq {MinBoardValue .. MaxBoardValue}) |> Seq.toList

    let calcWaysForSecondThrow (maxScore: int) (doubleValue: int) (secondThrowValue: int) =
        [(doubleValue + secondThrowValue) <= maxScore;
         (doubleValue + 2 * secondThrowValue) <= maxScore;
         (secondThrowValue <> OuterBullValue) && (doubleValue + 3 * secondThrowValue) <= maxScore] |> Seq.filter (fun value -> value) |> Seq.length

    let calcWaysForThirdThrow (maxScore: int) (doubleValue: int) (secondThrowValue: int) (thirdThrowValue: int) =
        [(doubleValue + secondThrowValue + thirdThrowValue) <= maxScore;
         (secondThrowValue < thirdThrowValue) && (doubleValue + secondThrowValue + 2 * thirdThrowValue) <= maxScore;
         (secondThrowValue < thirdThrowValue) && (thirdThrowValue <> OuterBullValue) && (doubleValue + secondThrowValue + 3 * thirdThrowValue) <= maxScore;
         (doubleValue + 2 * secondThrowValue + thirdThrowValue) <= maxScore;
         (doubleValue + 2 * secondThrowValue + 2 * thirdThrowValue) <= maxScore;
         (secondThrowValue < thirdThrowValue) && (thirdThrowValue <> OuterBullValue) && (doubleValue + 2 * secondThrowValue + 3 * thirdThrowValue) <= maxScore;
         (secondThrowValue <> OuterBullValue) && (doubleValue + 3 * secondThrowValue + thirdThrowValue) <= maxScore;
         (secondThrowValue <> OuterBullValue) && (doubleValue + 3 * secondThrowValue + 2 * thirdThrowValue) <= maxScore;
         (secondThrowValue <> OuterBullValue) && (thirdThrowValue <> OuterBullValue) && (doubleValue + 3 * secondThrowValue + 3 * thirdThrowValue) <= maxScore] |> Seq.filter (fun value -> value) |> Seq.length

    let rec processThirdThrow (maxScore: int) (board: int list) (doubleValue: int) (secondThrowValue: int) (result: int) =
        match board with
        | [] -> result
        | thirdThrowValue :: _ when doubleValue + secondThrowValue + thirdThrowValue > maxScore -> result
        | thirdThrowValue :: boardRest ->
            thirdThrowValue |> calcWaysForThirdThrow maxScore doubleValue secondThrowValue |> (+) result |> processThirdThrow maxScore boardRest doubleValue secondThrowValue

    let rec processSecondThrow (maxScore: int) (board: int list) (doubleValue: int) (result: int) =
        match board with
        | [] -> result
        | secondThrowValue :: _ when doubleValue + secondThrowValue > maxScore -> result
        | secondThrowValue :: boardRest ->
            secondThrowValue |> calcWaysForSecondThrow maxScore doubleValue |> (+) result |> processThirdThrow maxScore board doubleValue secondThrowValue |> processSecondThrow maxScore boardRest doubleValue

    let solveImpl (maxScore: int) =
        let board = generateBoard ()
        board |> Seq.filter (fun value -> 2 * value <= maxScore) |> Seq.map (fun value -> let doubleValue = 2 * value in 1 |> processSecondThrow maxScore board doubleValue) |> Seq.sum

    [<TestCase(99, 38182, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxScore: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxScore)
