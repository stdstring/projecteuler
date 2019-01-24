namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
// If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
// The lexicographic permutations of 0, 1 and 2 are: 012, 021, 102, 120, 201, 210
// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

[<TestFixture>]
type Problem024() =

    let solveImpl (lexicographicalNumber: int) (digitCount: int) =
        Permutations.GetPermutation(lexicographicalNumber - 1 |> bigint, [0 .. digitCount - 1]) |> Seq.map string |> String.concat ""

    [<TestCase(4, 3, "120", TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 10, "2783915460", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(lexicographicalNumber: int, digitCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, lexicographicalNumber, digitCount)