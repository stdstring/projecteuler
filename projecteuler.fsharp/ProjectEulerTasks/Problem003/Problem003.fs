namespace ProjectEulerTasks

open NUnit.Framework
open CommonLib
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem003() =

    let solveImpl (number: int64) =
        number |> NumbersDividers.GetPrimeDividers |> List.rev |> List.head

    [<TestCase(13195L, 29L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(600851475143L, 6857L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(number: int64, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, number)