namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

[<TestFixture>]
type Problem023() =

    let infimum = 12
    let supremum = 28123

    let solveImpl () =
        let abundantNumbers = seq {infimum .. supremum} |> Seq.filter(fun number -> (number |> NumbersDividers.GetDividers |> List.sum) - number > number) |> Seq.toList
        let abundantNumbersSet = HashSet<int>(abundantNumbers)
        let filterFun (number: int) =
            abundantNumbers |> List.takeWhile(fun abundantNumber -> abundantNumber < number) |> List.exists (fun abundantNumber -> abundantNumbersSet.Contains(number - abundantNumber)) |> not
        seq {1 .. supremum} |> Seq.filter filterFun |> Seq.sum

    [<TestCase(4179871, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)