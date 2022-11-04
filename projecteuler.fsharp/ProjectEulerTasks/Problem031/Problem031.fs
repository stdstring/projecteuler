namespace ProjectEulerTasks

open CommonLib.DynamicProgramming
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem031() =

    let solveImpl (value: int) (coins: int[]) =
        let partitions = SumPartitions<int>.CreateInt(value, coins)
        value |> partitions.GetPartitionCount

    [<TestCase(10, TimeThresholds.HardTimeLimit)>]
    member public this.SolveFor10p(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, 10, [|1; 2; 5|])

    [<TestCase(73682, TimeThresholds.HardTimeLimit)>]
    member public this.SolveFor2P(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, 200, [|1; 2; 5; 10; 20; 50; 100; 200|])
