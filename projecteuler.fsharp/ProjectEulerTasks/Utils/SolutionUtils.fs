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
        Assert.That(result, Is.EqualTo(expectedAnswer))
        Assert.That(stopwatch.ElapsedMilliseconds, Is.LessThanOrEqualTo(timeLimit))

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TIn -> 'TResult, input: 'TIn ) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input)

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TIn1 -> 'TIn2 -> 'TResult, input1: 'TIn1, input2: 'TIn2) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input1 input2)

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TIn1 -> 'TIn2 -> 'TIn3 -> 'TResult, input1: 'TIn1, input2: 'TIn2, input3: 'TIn3) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input1 input2 input3)

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TIn1 -> 'TIn2 -> 'TIn3 -> 'TIn4 -> 'TResult, input1: 'TIn1, input2: 'TIn2, input3: 'TIn3, input4: 'TIn4) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input1 input2 input3 input4)

    static member public CheckSolution(timeLimit: int, expectedAnswer: 'TResult, solver: 'TIn1 -> 'TIn2 -> 'TIn3 -> 'TIn4 -> 'TIn5 -> 'TResult, input1: 'TIn1, input2: 'TIn2, input3: 'TIn3, input4: 'TIn4, input5: 'TIn5) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solver input1 input2 input3 input4 input5)
