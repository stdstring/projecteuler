namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open CommonLib.Rational

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
