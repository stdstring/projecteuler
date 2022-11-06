namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem162Impl =
    type NumbersCountData = {Suitable: uint64; All: uint64; Without01A: uint64; Without01: uint64; Without0A: uint64; Without1A: uint64; Without0: uint64; Without1: uint64; WithoutA: uint64;}

open Problem162Impl

[<TestFixture>]
type Problem162() =

    // TODO (std_string) : add solution via combinatorics
    let solveImpl (maxSize: int) =
        let storage = Array.zeroCreate (maxSize + 1)
        storage.[0] <- {NumbersCountData.Suitable = 0UL;
                        NumbersCountData.All = 0UL;
                        NumbersCountData.Without01A = 1UL;
                        Without01 = 0UL;
                        Without0A = 0UL;
                        Without1A = 0UL;
                        Without0 = 0UL;
                        Without1 = 0UL;
                        WithoutA = 0UL}
        for number in 1 .. maxSize do
            let without01A = 13UL * storage.[number - 1].Without01A
            let without01 = 14UL * storage.[number - 1].Without01 + storage.[number - 1].Without01A
            let without0A = 14UL * storage.[number - 1].Without0A + storage.[number - 1].Without01A
            let without1A = 14UL * storage.[number - 1].Without1A + storage.[number - 1].Without01A
            let without0 = 15UL * storage.[number - 1].Without0 + storage.[number - 1].Without01 + storage.[number - 1].Without0A
            let without1 = 15UL * storage.[number - 1].Without1 + storage.[number - 1].Without01 + storage.[number - 1].Without1A
            let withoutA = 15UL * storage.[number - 1].WithoutA + storage.[number - 1].Without0A + storage.[number - 1].Without1A
            let all = 16UL * storage.[number - 1].All + storage.[number - 1].Without0 + storage.[number - 1].Without1 + storage.[number - 1].WithoutA
            let suitable = 15UL * storage.[number - 1].All + storage.[number - 1].Without1 + storage.[number - 1].WithoutA
            storage.[number] <- {NumbersCountData.Suitable = suitable;
                                 NumbersCountData.All = all;
                                 NumbersCountData.Without01A = without01A;
                                 NumbersCountData.Without01 = without01;
                                 NumbersCountData.Without0A = without0A;
                                 NumbersCountData.Without1A = without1A;
                                 NumbersCountData.Without0 = without0;
                                 NumbersCountData.Without1 = without1;
                                 NumbersCountData.WithoutA = withoutA}
        let result = storage |> Seq.sumBy (fun data -> data.Suitable)
        System.String.Format("{0:X}", result)

    [<TestCase(16, "3D58725572C62302", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxSize: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxSize)