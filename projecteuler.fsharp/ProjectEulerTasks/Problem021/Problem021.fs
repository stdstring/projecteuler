namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem021() =

    let solveImpl (maxNumber: int) =
        let numbersData = seq {2 .. maxNumber} |> Seq.map (fun value -> (value |> NumbersDividers.GetDividers |> List.sum) - value) |> Seq.toArray
        let foldFun (sum: int) (index: int) =
            let number = index + 2
            let otherNumber = numbersData.[index]
            let otherNumberIndex = otherNumber - 2
            match number with
            | _ when (otherNumberIndex >= 0) && (otherNumberIndex < numbersData.Length) && (numbersData.[otherNumberIndex] = number) && (otherNumber <> number) -> sum + number
            | _ -> sum
        seq {0 .. numbersData.Length - 1} |> Seq.fold foldFun 0

    [<TestCase(9999, 31626, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)