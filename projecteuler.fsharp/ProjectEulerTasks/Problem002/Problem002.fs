﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem002Impl =
    type FibonacciData = {Prev: int; Current: int}

open Problem002Impl

[<TestFixture>]
type Problem002() =

    let solveImpl (maxNumber: int) =
        let generator (data: FibonacciData) =
            let next = data.Prev + data.Current
            Some (next, {FibonacciData.Prev = data.Current; FibonacciData.Current = next})
        {FibonacciData.Prev = 0; FibonacciData.Current = 1} |> Seq.unfold generator |> Seq.takeWhile (fun number -> number <= maxNumber) |> Seq.filter (fun number -> number % 2 = 0) |> Seq.sum

    [<TestCase(99, 44, TimeThresholds.HardTimeLimit)>]
    [<TestCase(3999999, 4613732, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
