namespace ProjectEulerTasks

open NUnit.Framework
open CommonLib
open ProjectEulerTasks.Utils

// The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143 ?

[<TestFixture>]
type Problem003() =

    let solveImpl (number: int64) =
        number |> NumbersDividers.GetPrimeDividers |> List.rev |> List.head

    [<TestCase(13195L, 29L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(600851475143L, 6857L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(number: int64, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, number)