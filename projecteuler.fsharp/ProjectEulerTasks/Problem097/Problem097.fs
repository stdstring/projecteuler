namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem097() =

    let solveImpl (digitsCount: int) =
        let bound = pown 10L digitsCount
        let powerValue = seq { 1 .. 7830457 } |> Seq.fold (fun product _ -> product * 2L % bound) 1L
        let result = (28433L * powerValue + 1L) % bound |> string
        [String.replicate (digitsCount - result.Length) "0"; result] |> String.concat ""

    [<TestCase(10, "8739992577", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(digitsCount: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, digitsCount)