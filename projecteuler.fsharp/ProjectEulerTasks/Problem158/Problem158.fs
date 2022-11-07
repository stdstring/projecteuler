namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem158() =

    let solveImpl (alphabetSize: int) =
        // Description:
        // 1) C(n, k) - count of ways of selecting string with k length from n different characters
        // 2) Let we have string with k length sorted lexicographically (e.g., 'abc').
        //    Let we select one arbitrary character from this string and insert it before the rest of string; we can do it in C(k, 1) different ways.
        //    Let we select two arbitrary characters from this string (sort their lexicographically) and insert it before the rest of string; we can do it in C(k, 2) different ways.
        //    ...
        //    All these steps give us all strings in which exactly one character comes lexicographically after its neighbour to the left (except the one character sequence on each step which equals the source string).
        //    So total count of string in which exactly one character comes lexicographically after its neighbour to the left equals the following:
        //    (C(k, 1) - 1) + (C(k, 2) - 1) + ... + (C(k, k - 1) - 1)
        // 3) C(n, 0) + C(n, 1) + ... + C(n, n) = 2^n, C(n, 0) = C(n, n) = 1 => (C(k, 1) - 1) + (C(k, 2) - 1) + ... + (C(k, k - 1) - 1) = 2^k - (k + 1)
        // Here C(n, k) - binomial coefficient
        seq {2 .. alphabetSize} |> Seq.map (fun k -> Numbers.CalcBinomialCoeff(alphabetSize, k) * (pown 2I k - ((k |> bigint) + 1I)) |> int64) |> Seq.max

    [<TestCase(26, 409511334375L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(alphabetSize: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, alphabetSize)
