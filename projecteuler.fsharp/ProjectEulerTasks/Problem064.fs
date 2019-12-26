namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

// All square roots are periodic when written as continued fractions and can be written in the form:
// N^(1/2) = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))
// For example, we can write 23^(1/2) as [4;(1,3,1,8)].
// Here we use the notation 23^(1/2) = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.
// The first ten continued fraction representations of (irrational) square roots are:
// 2^(1/2) = [1;(2)], period = 1
// 3^(1/2) = [1;(1,2)], period = 2
// 5^(1/2) = [2;(4)], period = 1
// 6^(1/2) = [2;(2,4)], period = 2
// 7^(1/2) = [2;(1,1,1,4)], period = 4
// 8^(1/2) = [2;(1,4)], period = 2
// 10^(1/2) = [3;(6)], period = 1
// 11^(1/2) = [3;(3,6)], period = 2
// 12^(1/2) = [3;(2,6)], period = 2
// 13^(1/2) = [3;(1,1,1,1,6)], period = 5
// Exactly four continued fractions, for N <= 13, have an odd period.
// How many continued fractions for N <= 10000 have an odd period?

module Problem064Impl =
    type AData = {A0: int; AList: int list}

open Problem064Impl

[<TestFixture>]
type Problem064() =

    let m0 = 0
    let d0 = 1
    let a0Start = 1

    let processNumber (a0: int) (number: int) =
        let calcMDA (mPrev: int, dPrev: int, aPrev: int) =
            let m = dPrev * aPrev - mPrev
            let d = (number - m * m) / dPrev
            let a = (a0 + m) / d
            m, d, a
        let rec processNumberImpl (mdaPrev: int * int * int) (mdStorage: ISet<int * int>) (aStorage: int list) =
            let m, d, a = calcMDA mdaPrev
            match (m, d) |> mdStorage.Contains with
            | true -> {AData.A0 = a0; AData.AList = aStorage |> List.rev}
            | false ->
                (m, d) |> mdStorage.Add |> ignore
                processNumberImpl (m, d, a) mdStorage (a :: aStorage)
        match a0 with
        | _ when (a0 + 1) * (a0 + 1) = number -> {AData.A0 = (a0 + 1); AData.AList = []}
        | _ -> processNumberImpl (m0, d0, a0) (HashSet<int * int>()) []

    let solveImpl (maxNumber: int) =
        // TODO (std_string) : probably, move this algorithm into CommonLib
        // Algorithm (from https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion):
        // N - source number
        // m0 = 0, d0 = 1, a0 = integer(N^(1/2))
        // m = d_prev * a_prev - m_prev
        // d = (N - m * m) / d_prev
        // a = integer((a0 + m) / d)
        // Notice that m, d, and a are always integers. The algorithm terminates when this triplet is the same as one encountered before.
        // The expansion will repeat from then on. The sequence [a0; a1, a2, a3, ...] is the continued fraction expansion: N^(1/2) = a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))
        seq { 2 .. maxNumber } |>
        Seq.scan (fun aData number -> number |> processNumber aData.A0) {AData.A0 = a0Start; AData.AList = []} |>
        Seq.filter (fun aData -> aData.AList.Length % 2 = 1) |>
        Seq.length

    [<TestCase(13, 4, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10000, 1322, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)