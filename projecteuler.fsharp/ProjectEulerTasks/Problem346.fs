namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

// The number 7 is special, because 7 is 111 written in base 2, and 11 written in base 6. In other words, 7 is a repunit in at least two bases b > 1.
// We shall call a positive integer with this property a strong repunit. It can be verified that there are 8 strong repunits below 50: {1,7,13,15,21,31,40,43}.
// Furthermore, the sum of all strong repunits below 1000 equals 15864.
// Find the sum of all strong repunits below 1012.

[<TestFixture>]
type Problem346() =

    let generateRepunitForBase (maxNumber: int64) (numberBase: int64) (repunits: ISet<int64>) =
        let rec generateImpl (polynomValue: int64) (nextPolynomMember: int64) =
            match polynomValue < maxNumber with
            | false -> repunits
            | true ->
                if (polynomValue - 1L) > numberBase then
                    polynomValue |> repunits.Add |> ignore
                nextPolynomMember * numberBase |> generateImpl (polynomValue + nextPolynomMember)
        numberBase * numberBase |> generateImpl (numberBase + 1L)

    let solveImpl (maxNumber: int64) =
        // any n = 11 in base n - 1
        let maxNumberBase = maxNumber |> double |> sqrt |> int64
        seq {2L .. maxNumberBase} |> Seq.fold (fun repunits numberBase -> repunits |> generateRepunitForBase maxNumber numberBase) (new HashSet<int64>([1L]) :> ISet<int64>) |> Seq.sum

    [<TestCase(50L, 171L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000L, 15864L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000000000L, 336108797689259276L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int64, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)