﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The sequence of triangle numbers is generated by adding the natural numbers.
// So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
// The first ten terms would be: 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
// Let us list the factors of the first seven triangle numbers:
//  1: 1
//  3: 1, 3
//  6: 1, 2, 3, 6
// 10: 1, 2, 5, 10
// 15: 1, 3, 5, 15
// 21: 1, 3, 7, 21
// 28: 1, 2, 4, 7, 14, 28
// We can see that 28 is the first triangle number to have over five divisors.
// What is the value of the first triangle number to have over five hundred divisors?

module Problem012Impl =
    type TriangleData = {Prev: int; N: int}

open Problem012Impl

[<TestFixture>]
type Problem012() =

    let solveImpl (dividersCountInfimum: int) =
        let generator (data: TriangleData) =
            let nextN = data.N + 1
            let nextValue = data.Prev + nextN
            Some (nextValue, {TriangleData.Prev = nextValue; TriangleData.N = nextN})
        {TriangleData.Prev = 0; TriangleData.N = 0} |> Seq.unfold generator |> Seq.skipWhile (fun value -> NumbersDividers.GetDividers(value).Length < dividersCountInfimum) |> Seq.head


    [<TestCase(6, 28, TimeThresholds.HardTimeLimit)>]
    [<TestCase(501, 76576500, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dividersCountInfimum: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl dividersCountInfimum)