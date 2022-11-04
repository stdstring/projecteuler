namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib.Rational
open CommonLib

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