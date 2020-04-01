namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// We shall define a square lamina to be a square outline with a square "hole" so that the shape possesses vertical and horizontal symmetry.
// For example, using exactly thirty-two square tiles we can form two different square laminae: see picture here https://projecteuler.net/problem=173
// With one-hundred tiles, and not necessarily using all of the tiles at one time, it is possible to form forty-one different square laminae.
// Using up to one million tiles how many different square laminae can be formed?

[<TestFixture>]
type Problem173() =

    [<Literal>]
    let XMin = 2

    let calcArithProgressionPart (k: int) =
        let an = 2 * (k - 1)
        let n = an / 2
        (2 + an) * n / 2

    let rec calcSquareLaminaCount (n: int) (k: int) (count: int) =
        let arithProgressionPart = k |> calcArithProgressionPart
        match k * XMin + arithProgressionPart <= n with
        | true -> count + ((n - arithProgressionPart) / k) - XMin + 1 |> calcSquareLaminaCount n (k + 1)
        | false -> count

    let solveImpl (maxNumber: int) =
        // Description:
        // For all square laminas, minimum length of side = 2 => total cell count = 8
        // 1. Let square has side with length = x and width = 1, then total cell count = 4 * x
        // 2. Let square has side with width = 2 and length of interior layer = x, then total cell count = 4 * x + 4 * (x + 2) = 4 * 2 * x + 4 * 2
        // 3. Let square has side with width = 3 and length of interior layer = x, then total cell count = 4 * x + 4 * (x + 2) + 4 * (x + 4) = 4 * 3 * x + 4 * (2 + 4)
        // ...
        // k. Let square has side with width = k and length of interior layer = x, then total cell count = 4 * x + 4 * (x + 2) + ... + 4 * (x + 2 * (k - 1)) = 4 * k * x + 4 * (2 + 4 + ... + 2 * (k - 1))
        // ...
        // Let S(k) = 2 + 4 + ... + 2 * (k - 1), then 4 * k * x + 4 * S(k) <= maxNumber
        // Let n = int(maxNumber / 4) => k * x + S(k) <= n => x <= (n - S(k)) / k => xMax = int ((n - S(k)) / k) => 2 <= x <= int ((n - S(k)) / k) for given fixed k
        (maxNumber - 8) / 4 + 1 |> calcSquareLaminaCount (maxNumber / 4) 2

    [<TestCase(100, 41, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 1572729, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
