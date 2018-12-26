namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
// For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
// A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24.
// By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.
// However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number
// that cannot be expressed as the sum of two abundant numbers is less than this limit.
// Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

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
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl ())