namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
//
// 21 22 23 24 25
// 20  7  8  9 10
// 19  6  1  2 11
// 18  5  4  3 12
// 17 16 15 14 13
//
// It can be verified that the sum of the numbers on the diagonals is 101.
// What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

[<TestFixture>]
type Problem028() =

    let solveImpl (sideSize: int) =
        // Solution:
        // let SideSize - size of coil side, then N - coil number (beginning from 0) and SideSize = 2 * N + 1
        // Coil perimeter P = 4 * (2 * N + 1) - 4 = 8 * N (N > 0)
        // Volume (sum of perimeters of lower coils + central number 1) = 1 + Sum(8 * k, k = 1..N) = 1 + 4 * N * (N + 1)
        // For coil with number N corner numbers will be the following: V(N - 1) + 2 * N, V(N - 1) + 4 * N, V(N - 1) + 6 * N, V(N - 1) + 8 * N
        // So, sum of all diagonal numbers will be the following: 1 + Sum(4 * V(k - 1) + k * (2 + 4 + 6 + 8), k = 1 .. N)
        let n = (sideSize - 1) / 2
        1 + (4 * n) + (2 * n * (n + 1)) + (8 * n * (n + 1) * (2 * n + 1) / 3)

    [<TestCase(5, 101, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1001, 669171001, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(sideSize: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl sideSize)