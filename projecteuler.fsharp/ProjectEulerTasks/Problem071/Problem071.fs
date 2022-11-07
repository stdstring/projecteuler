namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem071() =

    let solveImpl (maxDenominator: int) (fractionNumerator: int) (fractionDenominator: int) =
        let findBestFraction (bestNumerator, bestDenominator) (numerator, denominator) =
            match numerator, denominator with
            | _ when bestNumerator * denominator < numerator * bestDenominator -> (numerator, denominator)
            | _ -> (bestNumerator, bestDenominator)
        seq { 2 .. maxDenominator } |>
        Seq.filter (fun denominator -> denominator % fractionDenominator <> 0) |>
        Seq.map (fun denominator -> fractionNumerator * denominator / fractionDenominator, denominator) |>
        Seq.fold findBestFraction (1, maxDenominator) |> fst

    [<TestCase(8, 3, 7, 2, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 3, 7, 428570, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxDenominator: int, fractionNumerator: int, fractionDenominator: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxDenominator, fractionNumerator, fractionDenominator)