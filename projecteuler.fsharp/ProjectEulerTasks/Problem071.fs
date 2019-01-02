namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// Consider the fraction, n/d, where n and d are positive integers. If n < d and HCF(n,d) = 1, it is called a reduced proper fraction.
// If we list the set of reduced proper fractions for d <= 8 in ascending order of size, we get:
// 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
// It can be seen that 2/5 is the fraction immediately to the left of 3/7.
// By listing the set of reduced proper fractions for d <= 1000000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.

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