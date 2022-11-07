namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem123() =

    // Description:
    // According to Binomial theorem (see here https://en.wikipedia.org/wiki/Binomial_theorem):
    // (a - 1)^n = a^n + C(1, n) * a^(n - 1) * (-1)  + C(2, n) * a^(n - 2) * (-1)^2 + ... + C(n - 2, n) * a^2 * (-1)^(n - 2) + C(n - 1, n) * a * (-1)^(n - 1) + (-1)^n
    // (a + 1)^n = a^n + C(1, n) * a^(n - 1)  + C(2, n) * a^(n - 2) + ... + C(n - 2, n) * a^2 + C(n - 1, n) * a + 1
    // for even n (a − 1)^n + (a + 1)^n = 2 * a^n + 2 * C(2, n) * a^(n - 2) + ... + 2 * C(n - 2, n) * a^2 + 2
    // for even n (a − 1)^n + (a + 1)^n mod a^2 = 2
    // for odd n (a − 1)^n + (a + 1)^n = 2 * a^n + 2 * C(2, n) * a^(n - 2) + ... + 2 * C(n - 3, n) * a^3 + 2 * C(n - 1, n) * a
    // for odd n (a − 1)^n + (a + 1)^n mod a^2 = 2 * C(n - 1, n) * a mod a^2 = 2 * a * n mod a^2
    // PS. C(n - 1, n) = n

    let SieveMaxNumber = 10000000

    let calcMaxRemainder (n: int) (pn: int) = 2L * (n |> int64) * (pn |> int64)

    let solveImpl (reminderMinValue: int64) =
        let sieve = SieveMaxNumber |> EratosSieve.Create
        // p1 = 2, p2 = 3, p3 = 5, p4 = 7, p5 = 11, ... Obviously, 2 * n * pn < pn^2 for n => 5 (pn => 11)
        sieve.ToSeq() |>
        Seq.mapi (fun index pn -> (index + 1), pn) |>
        Seq.skipWhile (fun (_, pn) -> pn < 11) |>
        Seq.filter (fun (n, _) -> n % 2 = 1) |>
        Seq.skipWhile (fun (n, pn) -> calcMaxRemainder n pn < reminderMinValue) |>
        Seq.head |>
        fst

    [<TestCase(1000000000L ,7037, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10000000000L ,21035, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(reminderMinValue: int64, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, reminderMinValue)
