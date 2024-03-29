﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem100() =

    let d = 2
    let c = -1

    let solveImpl (minTotalCount: int64) =
        // P(BB) = (Nb / T) * ((Nb - 1) / (T - 1)) = (Nb^2 - Nb) / (T^2 - T) = (Nb^2 - Nb + 1/4 - 1/4) / (T^2 - T + 1/4 - 1/4)
        // P(BB) = 1/2 => 2 * (Nb^2 - Nb + 1/4 - 1/4) = (T^2 - T + 1/4 - 1/4)
        // 2 * (Nb - 1/2)^2 - 1/2 = (T - 1/2)^2 - 1/4 => (T - 1/2)^2 - 2 * (Nb - 1/2)^2 = -1/4 =>
        // 4 * (T - 1/2)^2 - 4 * 2 * (Nb - 1/2)^2 = -1 => (2 * T - 1)^2 - 2 * (2 * Nb - 1)^2 = -1
        // X = 2 * T - 1, Y = 2 * Nb - 1 => X^2 - 2 * Y^2 = -1
        // X = 2 * T - 1, Y = 2 * B - 1 => X & Y must be odd
        let minTotalCount = minTotalCount |> bigint
        let firstSolution = PellEquation.FindFirstSolution(d, c).Value
        let rec findSolution (n: int) =
            let solution = PellEquation.FindNSolution(firstSolution, d, c, n)
            let totalCount = (solution.X + 1I) / 2I
            match totalCount with
            | _  when totalCount < minTotalCount -> findSolution (n + 2)
            | _ -> (solution.Y + 1I) / 2I
        findSolution 1 |> int64

    [<TestCase(21L, 15L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(22L, 85L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000000000L, 756872327473L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(minTotalCount: int64, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, minTotalCount)