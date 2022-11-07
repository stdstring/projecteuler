namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem004() =

    let tryFindFactor (factorMax: int) (factorMin: int) (number: int) =
        let factorMaxSquare = factorMax * factorMax
        match factorMaxSquare with
        | _ when factorMaxSquare < number -> false
        | _ -> (seq {factorMax .. -1 .. factorMin} |> Seq.skipWhile (fun factor -> factor * factorMin >= number) |> Seq.takeWhile (fun factor -> factor * factorMax >= number) |> Seq.tryFind (fun factor -> number % factor = 0)).IsSome

    let rec traverseNumber (factorMax: int) (factorMin: int) (numberBasis: int) =
        let basisDigits = NumbersDigits.GetDigits(numberBasis)
        let digits = basisDigits @ (basisDigits |> List.rev)
        let number = NumbersDigits.GetNumber(digits) |> int
        match tryFindFactor factorMax factorMin number with
        | false -> numberBasis - 1 |> traverseNumber factorMax factorMin
        | true -> number

    let solveImpl (factorDigitsCount: int) =
        let factorMin = pown 10 (factorDigitsCount - 1)
        let factorMax = (pown 10 factorDigitsCount) - 1
        traverseNumber factorMax factorMin factorMax

    [<TestCase(2, 9009, TimeThresholds.HardTimeLimit)>]
    [<TestCase(3, 906609, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(factorDigitsCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, factorDigitsCount)
