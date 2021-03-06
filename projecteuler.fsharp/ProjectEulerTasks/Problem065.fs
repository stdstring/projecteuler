﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib.Rational
open CommonLib

// The square root of 2 can be written as an infinite continued fraction.
// 2^(1/2) = 1 + 1 / (2 + 1 / (2 + 1 / (2 + 1 / (2 + ...))))
// The infinite continued fraction can be written, 2^(1/2) = [1;(2)], (2) indicates that 2 repeats ad infinitum.
// In a similar way, 23^(1/2) = [4;(1,3,1,8)].
// It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations.
// Let us consider the convergents for 2^(1/2).
// 1 + 1/2 = 3/2
// 1 + 1/(2 + 1/2) = 7/5
// 1 + 1/(2 + 1/(2 + 1/2)) = 17/12
// 1 + 1/(2 + 1/(2 + 1/(2 + 1/2)) = 41/29
// Hence the sequence of the first ten convergents for 2^(1/2) are:
// 1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
// What is most surprising is that the important mathematical constant, e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
// The first ten terms in the sequence of convergents for e are:
// 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
// The sum of digits in the numerator of the 10-th convergent is 1 + 4 + 5 + 7=17.
// Find the sum of digits in the numerator of the 100-th convergent of the continued fraction for e.

[<TestFixture>]
type Problem065() =

    let generateFractionRange (count: int) =
        let mapFun = function number when (number % 3 = 1) -> 1
                            | number when (number % 3 = 2) -> 2 * (1 + number / 3)
                            | number when (number % 3 = 0) -> 1
                            | _ -> failwith "Unexpected branch of match expression"
        seq {1 .. count} |> Seq.map mapFun |> Seq.rev

    let solveImpl (termCount: int) =
        let foldFun (value: RationalNumber) (term: int) =
            match value with
            | _ when value.IsZero -> new RationalNumber(term)
            | _ -> term + value.Reverse()
        let result = 2 + (termCount - 1 |> generateFractionRange |> Seq.fold foldFun (new RationalNumber(0))).Reverse()
        result.Numerator |> NumbersDigits.GetDigits |> List.sum

    [<TestCase(10, 17, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 272, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(termCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, termCount)