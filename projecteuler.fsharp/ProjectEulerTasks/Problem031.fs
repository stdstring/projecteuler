namespace ProjectEulerTasks

open CommonLib.DynamicProgramming
open NUnit.Framework
open ProjectEulerTasks.Utils

// In England the currency is made up of pound, P, and pence, p, and there are eight coins in general circulation: 1p, 2p, 5p, 10p, 20p, 50p, 1P (100p) and 2P (200p).
// It is possible to make 2P in the following way: 1*1P + 1*50p + 2*20p + 1*5p + 1*2p + 3*1p
// How many different ways can 2P be made using any number of coins?

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
