namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

[<TestFixture>]
type Problem046() =

     // TODO (std_string) : think about logic of choosing this top border
    let maxNumber = 9999

    let solveImpl () =
        let oddPrimes = EratosSieve.Create(maxNumber).ToSeq() |> Seq.skip 1 |> HashSet<int> :> ISet<int>
        let squares = seq {1 .. maxNumber} |> Seq.map (fun number -> number * number) |> Seq.takeWhile (fun number -> 2 * number <= maxNumber) |> Seq.toList
        let checkNumber (number: int) =
            squares |> Seq.takeWhile (fun square -> 2 * square <= number) |> Seq.exists(fun square -> (number - 2 * square) |> oddPrimes.Contains) |> not
        seq {3 .. 2 .. maxNumber} |> Seq.filter (fun number -> number |> oddPrimes.Contains |> not) |> Seq.filter checkNumber |> Seq.head

    [<TestCase(5777, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)