namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
// We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
// There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
// Find these four fractions.

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