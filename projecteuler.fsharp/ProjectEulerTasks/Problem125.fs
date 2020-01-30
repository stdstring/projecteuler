namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// The palindromic number 595 is interesting because it can be written as the sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.
// There are exactly eleven palindromes below one-thousand that can be written as consecutive square sums, and the sum of these palindromes is 4164.
// Note that 1 = 0^2 + 1^2 has not been included as this problem is concerned with the squares of positive integers.
// Find the sum of all the numbers less than 10^8 that are both palindromic and can be written as the sum of consecutive squares.

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
