namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
// 1/2 = 0.5
// 1/3 = 0.(3)
// 1/4 = 0.25
// 1/5 = 0.2
// 1/6 = 0.1(6)
// 1/7 = 0.(142857)
// 1/8 = 0.125
// 1/9 = 0.(1)
// 1/10 = 0.1
// Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
// Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

[<TestFixture>]
type Problem026() =

    let processNumber (number: int) =
        let storage = Array.create number -1
        let number = number |> bigint
        let rec processNumberImpl (numerator: bigint) (position: int) =
            match numerator % number with
            | reminder when reminder = 0I -> 0
            | reminder when storage.[(reminder |> int) - 1] <> - 1 -> position - storage.[(reminder |> int) - 1]
            | reminder ->
                storage.[(reminder |> int) - 1]<-position
                (position + 1) |> processNumberImpl (10I * numerator)
        1 |> processNumberImpl 10I

    let solveImpl (maxDenominator: int) =
        // Solution:
        // 1) For number N there are N distinct rests: 0, 1, ..., N-1
        // 2) Let Lp - length of non recurring prefix, Lc - length of recurring cycle in the decimal fraction part
        // For number N : 10^n < N < 10^(n + 1) we can state the following:
        // Rem1 = 10^(n + 1) rem N, Rem2 = 10^(n + 2) rem N, ...Remp = 10^(n + Lp) rem N, RemC1 = 10^(n + Lp + 1) rem N, ..., RemCN = 10^(n + Lp + Lc) rem N
        // Rem1 <> Rem2 <> ..., RemC1 = RemCn, Lp + Lc < = N
        seq {2 .. maxDenominator} |> Seq.maxBy (fun denominator -> denominator |> processNumber)

    [<TestCase(10, 7, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 983, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxDenominator: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl maxDenominator)