namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// 2^7=128 is the first power of two whose leading digits are "12".
// The next power of two whose leading digits are "12" is 2^80.
// Define p(L,n) to be the nth-smallest value of j such that the base 10 representation of 2^j begins with the digits of L.
// So p(12,1)=7 and p(12,2)=80.
// You are also given that p(123,45)=12710. Find p(123,678910).

[<TestFixture>]
type Problem686() =

    let log2Value = 2.0 |> System.Math.Log10

    // TODO (std_string) : probably, move it into CommonLib
    let extractFractionalPart (value: double) = value - truncate value

    let solveImpl (expectedPrefix: int) (n: int) =
        let factor =  pown 10 (expectedPrefix |> double |> System.Math.Log10 |> int) |> double
        let leftBorderValue = 10 * expectedPrefix - 1 |> double |> System.Math.Log10 |> extractFractionalPart
        let rightBorderValue = 10 * expectedPrefix + 11 |> double |> System.Math.Log10 |> extractFractionalPart
        let mutable power = 3
        let mutable memberNumber = 0
        while memberNumber < n do
            power <- power + 1
            let fractionalExponentFor10 = (power |> double) * log2Value |> extractFractionalPart
            if leftBorderValue < fractionalExponentFor10 && fractionalExponentFor10 < rightBorderValue then
                let actualPrefix = (10.0 ** fractionalExponentFor10) * factor |> int
                if actualPrefix = expectedPrefix then
                    memberNumber <- memberNumber + 1
        power

    [<TestCase(12, 1, 7, TimeThresholds.HardTimeLimit)>]
    [<TestCase(12, 2, 80, TimeThresholds.HardTimeLimit)>]
    [<TestCase(123, 45, 12710, TimeThresholds.HardTimeLimit)>]
    [<TestCase(123, 678910, 193060223, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(prefix: int, n: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, prefix, n)
