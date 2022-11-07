namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem301() =

    // TODO (std_string): think about smarter and faster solution
    let solveImpl (maxNumber: int) =
        let maxNumber = maxNumber |> uint32
        let mutable count = 0
        for number in seq {1u .. maxNumber} do
            if (number ^^^ (2u * number) ^^^ (3u * number)) = 0u then
                count <- count + 1
        count

    [<TestCase(1073741824, 2178309, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)