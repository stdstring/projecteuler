﻿namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem001() =

    let arithProgressionSum (a1: int) (an: int) (n: int) =
        n * (a1 + an) / 2

    let solveImpl (divider1: int) (divider2: int) (maxNumber: int) =
        let sum1 = arithProgressionSum divider1 (maxNumber - maxNumber % divider1) (maxNumber / divider1)
        let sum2 = arithProgressionSum divider2 (maxNumber - maxNumber % divider2) (maxNumber / divider2)
        let lcm = NumbersRelation.CalcLCM(divider1, divider2)
        let sum12 = arithProgressionSum (lcm) (maxNumber - maxNumber % (lcm)) (maxNumber / (lcm))
        sum1 + sum2 - sum12

    [<TestCase(3, 5, 9, 23, TimeThresholds.HardTimeLimit)>]
    [<TestCase(3, 5, 999, 233168, TimeThresholds.HardTimeLimit)>]
    [<TestCase(2, 4, 9, 20, TimeThresholds.HardTimeLimit)>]
    [<TestCase(4, 6, 19, 64, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(divider1: int, divider2: int, maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, divider1, divider2, maxNumber)