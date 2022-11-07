namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

[<TestFixture>]
type Problem013() =

    let solveImpl (dataFilename: string) (count: int) =
        let sum = File.ReadLines(Path.Combine("Data", dataFilename)) |> Seq.map (fun line -> line |> bigint.Parse) |> Seq.sum
        (sum |> string).Substring(0, count)

    [<TestCase("problem_013.dat", 10, "5537376230", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, count: int, expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename, count)
