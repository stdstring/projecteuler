namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem073() =

    let processDenominator (leftNumerator: int) (leftDenominator: int) (rightNumerator: int) (rightDenominator: int) (denominator: int) =
        let calcNumeratorRange () =
            let minNumerator = leftNumerator * denominator / leftDenominator + 1
            match rightNumerator * denominator % rightDenominator with
            | 0 -> minNumerator, rightNumerator * denominator / rightDenominator - 1
            | _ -> minNumerator, rightNumerator * denominator / rightDenominator
        let minNumerator, maxNumerator = calcNumeratorRange ()
        let numeratorSeq = match denominator % 2, minNumerator % 2 with
                           | 0, 0 -> seq { minNumerator + 1 .. 2 .. maxNumerator }
                           | 0, 1 -> seq { minNumerator .. 2 .. maxNumerator }
                           | _ -> seq { minNumerator .. maxNumerator }
        numeratorSeq |> Seq.filter (fun numerator -> NumbersRelation.CalcGCD(denominator, numerator) = 1) |> Seq.length

    let solveImpl (leftNumerator: int) (leftDenominator: int) (rightNumerator: int) (rightDenominator: int) (maxDenominator: int) =
        seq { 2 .. maxDenominator } |> Seq.map (fun denominator -> denominator |> processDenominator leftNumerator leftDenominator rightNumerator rightDenominator) |> Seq.sum

    [<TestCase(1, 3, 1, 2, 8, 3, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1, 3, 1, 2, 12000, 7295372, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(leftNumerator: int, leftDenominator: int, rightNumerator: int, rightDenominator: int, maxDenominator: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, leftNumerator, leftDenominator, rightNumerator, rightDenominator, maxDenominator)