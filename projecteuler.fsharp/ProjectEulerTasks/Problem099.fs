namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

// Comparing two numbers written in index form like 2^11 and 3^7 is not difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
// However, confirming that 632382^518061 > 519432^525806 would be much more difficult, as both numbers contain over three million digits.
// Using problem_099.dat, a data file containing one thousand lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.
// NOTE: The first two lines in the file represent the numbers in the example given above.

[<TestFixture>]
type Problem099() =

    let solveImpl (dataFilename: string) =
        let lineNumber, _, _ = File.ReadAllLines(Path.Combine("Data", dataFilename)) |>
                               Seq.mapi (fun index line -> let parts = line.Split(',') in (index + 1), parts.[0] |> int, parts.[1] |> int) |>
                               Seq.maxBy (fun (_, baseValue, powerValue) -> powerValue |> float |> (*) (baseValue |> float |> log10))
        lineNumber

    [<TestCase("problem_099.dat", 709, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)