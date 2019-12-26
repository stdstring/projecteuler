namespace ProjectEulerTasks

open CommonLib.DynamicProgramming
open NUnit.Framework
open ProjectEulerTasks.Utils

// It is possible to write five as a sum in exactly six different ways:
// 4 + 1
// 3 + 2
// 3 + 1 + 1
// 2 + 2 + 1
// 2 + 1 + 1 + 1
// 1 + 1 + 1 + 1 + 1
// How many different ways can one hundred be written as a sum of at least two positive integers?

[<TestFixture>]
type Problem076() =

    let solveImpl (number: int) =
        let partitions = SumPartitions<int>.CreateInt(number, [|1 .. number - 1|])
        number |> partitions.GetPartitionCount

    [<TestCase(5, 6, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 190569291, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(number: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, number)

