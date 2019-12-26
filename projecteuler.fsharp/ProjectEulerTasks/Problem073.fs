namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// Consider the fraction, n/d, where n and d are positive integers. If n < d and HCF(n,d)=1, it is called a reduced proper fraction.
// If we list the set of reduced proper fractions for d <= 8 in ascending order of size, we get:
// 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
// It can be seen that there are 3 fractions between 1/3 and 1/2: 3/8, 2/5, 3/7.
// How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d <= 12000?

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