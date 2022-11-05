namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

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