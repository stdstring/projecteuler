namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

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