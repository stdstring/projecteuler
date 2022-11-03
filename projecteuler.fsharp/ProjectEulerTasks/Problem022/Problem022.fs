namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System
open System.Text

[<TestFixture>]
type Problem022() =

    let asciiEncoder = Encoding.ASCII

    let start = asciiEncoder.GetBytes("A").[0]

    let getValue (source: string) =
        source |> asciiEncoder.GetBytes |> Seq.map (fun byteValue -> byteValue - start + 1uy |> int) |> Seq.sum

    let solveImpl (dataFilename: string) =
        let names = File.ReadAllText(Path.Combine("Data", dataFilename)).Split([|','; '"'|], StringSplitOptions.RemoveEmptyEntries) |> Array.sort
        seq{0 .. names.Length - 1} |> Seq.map (fun index -> names.[index] |> getValue |> (*) (index + 1)) |> Seq.sum

    [<TestCase("problem_022.dat", 871198282, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)