﻿namespace ProjectEulerTasks.Utils

open System.Diagnostics
open NUnit.Framework

[<AbstractClass; Sealed>]
type SolutionUtils =

    static member public CheckSolution<'TResult>(timeLimit: int, expectedAnswer: 'TResult, solver: unit -> 'TResult) =
        let stopwatch = Stopwatch.StartNew()
        let result = solver ()
        Assert.AreEqual(expectedAnswer, result)
        stopwatch.Stop()
        printfn "Execution time is %i ms" stopwatch.ElapsedMilliseconds
        Assert.LessOrEqual(stopwatch.ElapsedMilliseconds, timeLimit)