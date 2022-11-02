namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// There are exactly ten ways of selecting three from five, 12345: 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
// In combinatorics, we use the notation, C(5, 3) = 10.
// In general, C(n, r) = n! / (r! * (n - r)!), where r <= n, n! = n * (n-1) * ... * 3 * 2 * 1, and 0! = 1.
// It is not until n = 23, that a value exceeds one-million: C(23, 10) = 1144066.
// How many, not necessarily distinct, values of  C(n, r), for 1 <= n <= 100, are greater than one-million?

[<TestFixture>]
type Problem053() =

    let calcPascalTriangleRow (n: int) (prevRow: ResizeArray<bigint>) =
        let row = ResizeArray<bigint>()
        row.Add(1I)
        seq { 1 .. n - 1 } |> Seq.iter (fun index -> row.Add(prevRow.[index - 1] + prevRow.[index]))
        row.Add(1I)
        row

    let solveImpl (maxN: int) (infimumValue: int) =
        let infimumValue = infimumValue |> bigint
        let initRow = ResizeArray<bigint>([1I])
        let calcValuesCount (row: ResizeArray<bigint>) = row |> Seq.filter(fun value -> value >= infimumValue) |> Seq.length
        seq { 1 .. maxN } |> Seq.fold (fun (prevCount, prevRow) n -> let row = prevRow |> calcPascalTriangleRow n in (row |> calcValuesCount |> (+) prevCount, row)) (0, initRow) |> fst

    [<TestCase(22, 1000001, 0, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 1000001, 4075, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxN: int, infimumValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxN, infimumValue)