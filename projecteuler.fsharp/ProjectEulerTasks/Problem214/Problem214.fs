namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem214() =

    let rec processChain (eulerTotientFunction: LazyEulerTotientFunction) (length: int) (number: int) =
        match number with
        | 1 -> length + 1
        | _ -> number |> eulerTotientFunction.GetValue |> processChain eulerTotientFunction (length + 1)

    let solveImpl (maxNumber: int) (chainLength: int) =
        let eulerTotientFunction = maxNumber |> LazyEulerTotientFunction.Create
        let mutable result = 0L
        for prime in eulerTotientFunction.GetPrimes() do
            if (prime |> processChain eulerTotientFunction 0) = chainLength then
                result<-result + (prime |> int64)
        result |> int64

    [<TestCase(20, 4, 12L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(40000000, 25, 1677366278943L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, chainLength: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, chainLength)
