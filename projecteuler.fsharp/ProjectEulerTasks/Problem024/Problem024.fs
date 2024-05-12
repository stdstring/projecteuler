namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem024() =

    let solveImpl (lexicographicalNumber: int) (digitCount: int) =
        match Permutations.GetPermutation(lexicographicalNumber - 1 |> bigint, [0 .. digitCount - 1]) with
        | Some permutation -> permutation |> Seq.map string |> String.concat ""
        | None -> failwith "Unexpected branch of match expression"

    [<TestCase(4, 3, "120", TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 10, "2783915460", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(lexicographicalNumber: int, digitCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, lexicographicalNumber, digitCount)