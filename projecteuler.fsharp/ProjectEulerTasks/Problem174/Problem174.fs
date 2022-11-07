namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

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
