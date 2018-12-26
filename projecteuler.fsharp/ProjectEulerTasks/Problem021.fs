namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
// If d(a) = b and d(b) = a, where a != b, then a and b are an amicable pair and each of a and b are called amicable numbers.
// For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
// The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
// Evaluate the sum of all the amicable numbers under 10000.

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
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl maxNumber)