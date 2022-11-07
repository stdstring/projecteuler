namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem033() =

    [<Literal>]
    let StartNumerator = 11

    [<Literal>]
    let MaxNumerator = 49

    [<Literal>]
    let MaxDenominator = 99

    let checkSimplification (numerator: int) (numeratorDigits: int list) (denominator: int) =
        let denominatorDigits = denominator |> NumbersDigits.GetDigits
        match numeratorDigits, denominatorDigits with
        | [digit1; digit2], [digit3; digit4] when (digit1 = digit3) && (numerator * digit4 = denominator * digit2) -> (digit2, digit4) |> Some
        | [digit1; digit2], [digit3; digit4] when (digit1 = digit4) && (numerator * digit3 = denominator * digit2) -> (digit2, digit3) |> Some
        | [digit1; digit2], [digit3; digit4] when (digit2 = digit3) && (numerator * digit4 = denominator * digit1) -> (digit1, digit4) |> Some
        | [digit1; digit2], [digit3; digit4] when (digit2 = digit4) && (numerator * digit3 = denominator * digit1) -> (digit1, digit3) |> Some
        | _ -> None

    let processDenominators (numerator: int) =
        let numeratorDigits = numerator |> NumbersDigits.GetDigits
        seq {2 * numerator .. MaxDenominator} |> Seq.choose (fun denominator -> denominator |> checkSimplification numerator numeratorDigits) |> Seq.toList

    let solveImpl () =
        seq {StartNumerator .. MaxNumerator} |> Seq.filter (fun numerator -> numerator % 10 <> 0) |> Seq.map (fun numerator -> numerator |> processDenominators) |> List.concat

    [<TestCase(TimeThresholds.HardTimeLimit)>]
    member public this.Solve(timeLimit: int) =
        let expectedAnswer = [(1, 4); (1, 5); (2, 5); (4, 8)]
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl ())