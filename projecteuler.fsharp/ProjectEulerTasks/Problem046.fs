namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
// 9 = 7 + 2 * 1^2
// 15 = 7 + 2 * 2^2
// 21 = 3 + 2 * 3^2
// 25 = 7 + 2 * 3^2
// 27 = 19 + 2 * 2^2
// 33 = 31 + 2 * 1^2
// It turns out that the conjecture was false. What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

[<TestFixture>]
type Problem046() =

     // TODO (std_string) : think about logic of choosing this top border
    let maxNumber = 9999

    let solveImpl () =
        let sieveBuilder = EratosSieveBuilder()
        let oddPrimes = sieveBuilder.CreateSieve(maxNumber).ToSeq() |> Seq.skip 1 |> HashSet<int> :> ISet<int>
        let squares = seq {1 .. maxNumber} |> Seq.map (fun number -> number * number) |> Seq.takeWhile (fun number -> 2 * number <= maxNumber) |> Seq.toList
        let checkNumber (number: int) =
            squares |> Seq.takeWhile (fun square -> 2 * square <= number) |> Seq.exists(fun square -> (number - 2 * square) |> oddPrimes.Contains) |> not
        seq {3 .. 2 .. maxNumber} |> Seq.filter (fun number -> number |> oddPrimes.Contains |> not) |> Seq.filter checkNumber |> Seq.head

    [<TestCase(5777, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)