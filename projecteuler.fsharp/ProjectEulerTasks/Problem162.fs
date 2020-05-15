namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// In the hexadecimal number system numbers are represented using 16 different digits: 0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F
// The hexadecimal number AF when written in the decimal number system equals 10 * 16 + 15 = 175.
// In the 3-digit hexadecimal numbers 10A, 1A0, A10, and A01 the digits 0,1 and A are all present.
// Like numbers written in base ten we write hexadecimal numbers without leading zeroes.
// How many hexadecimal numbers containing at most sixteen hexadecimal digits exist with all of the digits 0,1, and A present at least once?
// Give your answer as a hexadecimal number.
// (A,B,C,D,E and F in upper case, without any leading or trailing code that marks the number as hexadecimal and without leading zeroes , e.g. 1A3F and not: 1a3f and not 0x1a3f and not $1A3F and not #1A3F and not 0000001A3F)

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