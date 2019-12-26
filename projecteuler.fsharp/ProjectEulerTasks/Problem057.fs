namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open CommonLib.Rational

// It is possible to show that the square root of two can be expressed as an infinite continued fraction.
// 2^(1/2) = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
// By expanding this for the first four iterations, we get:
// 1 + 1/2 = 3/2 = 1.5
// 1 + 1/(2 + 1/2) = 7/5 = 1.4
// 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
// 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
// The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985,
// is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
// In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

[<TestFixture>]
type Problem057() =

    let calcNextValue (prevValue: RationalNumber) =
        (1 + (1 + prevValue).Reverse()).Simplify()

    let checkValue (value: RationalNumber) =
        (value.Numerator |> NumbersDigits.GetDigits |> List.length) > (value.Denominator |> NumbersDigits.GetDigits |> List.length)

    let solveImpl (iterationCount: int) =
        let generator (iteration: int, prevValue: RationalNumber) =
            match iteration with
            | _ when iteration > iterationCount -> None
            | _ ->
                let nextValue = prevValue |> calcNextValue
                let nextIteration = iteration + 1
                (nextValue, (nextIteration, nextValue)) |> Some
        (1, new RationalNumber(3, 2)) |> Seq.unfold generator |> Seq.filter checkValue |> Seq.length

    [<TestCase(1000, 153, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(iterationCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, iterationCount)
