﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem017() =

    let from0To9 = [|""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"|]
    let from10To19 = [|"ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
    let otherTens = [|"twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"|]

    let rec processNumber (number: int) =
        match number with
        | _ when (1000 <= number) && (number <= 9999) -> from0To9.[number / 1000].Length + "thousand".Length + processNumber (number % 1000)
        | 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900 -> from0To9.[number / 100].Length + "hundred".Length
        | _ when (100 <= number) && (number <= 999) -> from0To9.[number / 100].Length + "hundred".Length + "and".Length + processNumber (number % 100)
        | _ when (20 <= number) && (number <= 99) -> otherTens.[number / 10 - 2].Length + processNumber (number % 10)
        | _ when (10 <= number) && (number <= 19) -> from10To19.[number - 10].Length
        | _ when (0 <= number) && (number <= 9) -> from0To9.[number].Length
        | _ -> failwith "Unsupported number"

    let solveImpl (maxNumber: int) =
        seq {1 .. maxNumber} |> Seq.map (fun number -> number |> processNumber) |> Seq.sum

    [<TestCase(5, 19, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 21124, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        10 |> processNumber |> printfn "%d"
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)