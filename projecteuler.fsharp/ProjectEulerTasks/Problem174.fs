namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// We shall define a square lamina to be a square outline with a square "hole" so that the shape possesses vertical and horizontal symmetry.
// Given eight tiles it is possible to form a lamina in only one way: 3x3 square with a 1x1 hole in the middle.
// However, using thirty-two tiles it is possible to form two distinct laminae: see picture here https://projecteuler.net/problem=174
// If t represents the number of tiles used, we shall say that t = 8 is type L(1) and t = 32 is type L(2).
// Let N(n) be the number of t <= 1000000 such that t is type L(n); for example, N(15) = 832.
// What is Sum N(n) for 1 <= n <= 10?

[<TestFixture>]
type Problem174() =

    [<Literal>]
    let XMin = 2

    [<Literal>]
    let TypeMin = 1

    let calcArithProgressionPart (k: int) =
        let an = 2 * (k - 1)
        let n = an / 2
        (2 + an) * n / 2

    let rec fillSquareLaminaTypeStorage (n: int) (k: int) (storage: int[]) =
        let arithProgressionPart = k |> calcArithProgressionPart
        match k * XMin + arithProgressionPart <= n with
        | true ->
            let xMax = (n - arithProgressionPart) / k
            seq {XMin .. xMax} |> Seq.map (fun x -> 4 * (k * x + arithProgressionPart)) |> Seq.iter (fun cells -> storage.[cells] <- storage.[cells] + 1)
            storage |> fillSquareLaminaTypeStorage n (k + 1)
        | false -> storage

    let solveImpl (maxNumber: int) (maxType: int) =
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
        let storage = maxNumber + 1 |> Array.zeroCreate
        // process square lamina with width = 1
        seq {8 .. 4 .. maxNumber} |> Seq.iter (fun cells -> storage.[cells] <- 1)
        // process square lamina with width > 1 and calc Sum N(n)
        storage |> fillSquareLaminaTypeStorage (maxNumber / 4) 2 |> Seq.skip (XMin * 4) |> Seq.filter (fun currentType -> currentType >= TypeMin && currentType <= maxType) |> Seq.length

    [<TestCase(1000000, 10, 209566, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, maxType: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, maxType)
