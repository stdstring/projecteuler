﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// An irrational decimal fraction is created by concatenating the positive integers: 0.123456789101112131415161718192021...
// It can be seen that the 12th digit of the fractional part is 1.
// If dn represents the nth digit of the fractional part, find the value of the following expression: d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000

[<TestFixture>]
type Problem040() =

    let calculateDigit (position: int) =
        let index = position - 1
        let rec calculateDigitImpl (rangeStart: int) (rangeCount: int) (digitsCount: int) (totalLength: int) =
            let updatedTotalLength = totalLength + rangeCount * digitsCount
            match updatedTotalLength with
            | _ when index < updatedTotalLength ->
                let number = rangeStart + (index - totalLength) / digitsCount
                number |> NumbersDigits.GetDigits |> List.item ((index - totalLength) % digitsCount)
            | _ -> calculateDigitImpl (rangeStart * 10) (rangeCount * 10) (digitsCount + 1) updatedTotalLength
        calculateDigitImpl 1 9 1 0

    let solveImpl () =
        [1; 10; 100; 1000; 10000; 100000; 1000000] |> Seq.map calculateDigit |> Seq.fold (fun product digit -> product * digit) 1

    [<TestCase(210, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl ())