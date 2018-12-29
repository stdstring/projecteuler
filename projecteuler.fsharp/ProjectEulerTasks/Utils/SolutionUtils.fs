namespace ProjectEulerTasks.Utils

open System.Diagnostics
open NUnit.Framework

[<AbstractClass; Sealed>]
type SolutionUtils =

    static member public CheckSolution<'TResult>(timeLimit: int, expectedAnswer: 'TResult, solver: unit -> 'TResult) =
        let stopwatch = Stopwatch.StartNew()
        let result = solver ()
        stopwatch.Stop()
        printfn "Execution time is %i ms" stopwatch.ElapsedMilliseconds
        Assert.AreEqual(expectedAnswer, result)
        Assert.LessOrEqual(stopwatch.ElapsedMilliseconds, timeLimit)

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TInput -> 'TResult, input: 'TInput ) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input)

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TInput1 -> 'TInput2 -> 'TResult, input1: 'TInput1, input2: 'TInput2) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input1 input2)

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TInput1 -> 'TInput2 -> 'TInput3 -> 'TResult, input1: 'TInput1, input2: 'TInput2, input3: 'TInput3) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input1 input2 input3)
