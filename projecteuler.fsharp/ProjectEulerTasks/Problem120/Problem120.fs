namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem120() =

    // Description:
    // According to Binomial theorem (see here https://en.wikipedia.org/wiki/Binomial_theorem):
    // (a - 1)^n = a^n + C(1, n) * a^(n - 1) * (-1)  + C(2, n) * a^(n - 2) * (-1)^2 + ... + C(n - 2, n) * a^2 * (-1)^(n - 2) + C(n - 1, n) * a * (-1)^(n - 1) + (-1)^n
    // (a + 1)^n = a^n + C(1, n) * a^(n - 1)  + C(2, n) * a^(n - 2) + ... + C(n - 2, n) * a^2 + C(n - 1, n) * a + 1
    // for even n (a − 1)^n + (a + 1)^n = 2 * a^n + 2 * C(2, n) * a^(n - 2) + ... + 2 * C(n - 2, n) * a^2 + 2
    // for even n (a − 1)^n + (a + 1)^n mod a^2 = 2
    // for odd n (a − 1)^n + (a + 1)^n = 2 * a^n + 2 * C(2, n) * a^(n - 2) + ... + 2 * C(n - 3, n) * a^3 + 2 * C(n - 1, n) * a
    // for odd n (a − 1)^n + (a + 1)^n mod a^2 = 2 * C(n - 1, n) * a mod a^2 = 2 * a * n mod a^2 => 2 * a * nmax < a^2 => 2 * nmax < a
    // We can show easily, that nmax = trunc((a - 1) / 2)
    // PS. C(n - 1, n) = n

    let calcMaxRemainder (a: int) = 2 * a * ((a - 1) / 2)

    let solveImpl (minValue: int) (maxValue: int) =
        seq {minValue .. maxValue} |> Seq.map (fun a -> a |> calcMaxRemainder) |> Seq.sum

    [<TestCase(3, 1000, 333082500, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(minValue: int, maxValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, minValue, maxValue)
