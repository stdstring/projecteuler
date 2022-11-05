namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem125() =

    let generateSquares (maxNumber: int) =
        let maxRootNumber = 1 + (maxNumber |> double |> sqrt |> int)
        seq {1 .. maxRootNumber} |> Seq.map (fun number -> number * number) |> Seq.filter (fun number -> number < maxNumber) |> Seq.toArray

    let isPalindrome (value: int) =
        let digits = value |> NumbersDigits.GetDigits
        digits = (digits |> List.rev)

    let processSum (maxNumber: int) (squares: int[]) (processedNumbers: bool[]) (start: int) =
        let rec processSumImpl (current: int) (sum: int) (result: int64) =
            match current with
            | _ when current > squares.Length - 1 -> result
            | _ ->
                match sum + squares.[current] with
                | value when value > maxNumber -> result
                | value ->
                    match (isPalindrome value) && (processedNumbers.[value] |> not) with
                    | true ->
                        processedNumbers.[value] <- true
                        result + (value |> int64) |> processSumImpl (current + 1) value
                    | false -> result |> processSumImpl (current + 1) value
        0L |> processSumImpl (start + 1) (squares.[start])

    let solveImpl (maxNumber: int) =
        let squares = maxNumber |> generateSquares
        let processedNumbers = Array.create (maxNumber + 1) false
        seq {0 .. squares.Length - 2} |> Seq.map (fun start -> start |> processSum maxNumber squares processedNumbers) |> Seq.sum

    [<TestCase(1000, 4164L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100000000, 2906969179L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
