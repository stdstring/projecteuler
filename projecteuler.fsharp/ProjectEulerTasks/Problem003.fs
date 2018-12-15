namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143 ?

[<TestFixture>]
type Problem003() =

    let rec clearFromDivider (number: int64) (divider: int64) =
        match number % divider with
        | 0L -> clearFromDivider (number / divider) divider
        | _ -> number

    let addDividerIfPrime (divider: int64) (primeDividers: List<int64>) =
        match primeDividers |> List.exists (fun primeDivider -> divider / primeDivider = 0L) with
        | true -> primeDividers
        | false -> divider :: primeDividers

    let rec processNumber (number: int64) (divider: int64) (primeDividers: List<int64>) =
        match number with
        | _ when divider * divider > number -> number :: primeDividers
        | _ when divider * divider = number -> primeDividers |> addDividerIfPrime divider
        | _ when number % divider = 0L -> primeDividers |> addDividerIfPrime divider |> processNumber (clearFromDivider number divider) (divider + 2L)
        | _ -> primeDividers |> processNumber number (divider + 2L)

    let solveImpl (number: int64) =
        match (if number % 2L = 0L then processNumber (clearFromDivider number 2L) 3L [2L] else processNumber number 3L []) with
        | [] -> number
        | divider :: _ -> divider

    [<TestCase(13195L, 29L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(600851475143L, 6857L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(number: int64, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl number)