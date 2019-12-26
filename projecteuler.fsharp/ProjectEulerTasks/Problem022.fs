namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System
open System.Text

// Using "problem_022.dat", a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order.
// Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
// For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
// So, COLIN would obtain a score of 938 * 53 = 49714.
// What is the total of all the name scores in the file?

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