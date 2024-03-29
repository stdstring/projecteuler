﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem015() =

    let solveImpl (gridSize: int) =
        // Solution for N * N grid:
        // 1) each path contains N horizontal and N vertical regions
        // 2) It is enough N horizontal regions (or N horizontal regions) for path definition
        // So for solution we may calculate number of combinations by N from 2 * N (calculate binomial coefficient)
        Numbers.CalcBinomialCoeff(2 * gridSize, gridSize) |> int64

    [<TestCase(2, 6L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(3, 20L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(20, 137846528820L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(gridSize: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, gridSize)
