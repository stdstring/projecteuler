namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System
open System.Text
open System.Collections.Generic

[<TestFixture>]
type Problem042() =

    let maxTriangleNumber = 200

    let asciiEncoder = Encoding.ASCII

    let start = asciiEncoder.GetBytes("A").[0]

    let calcWordValue (word: string) = word |> asciiEncoder.GetBytes |> Seq.map (fun byteValue -> byteValue - start + 1uy |> int) |> Seq.sum

    let solveImpl (dataFilename: string) =
        let names = File.ReadAllText(Path.Combine("Data", dataFilename)).Split([|','; '"'|], StringSplitOptions.RemoveEmptyEntries)
        let triangleNumbers = seq {1 .. maxTriangleNumber} |> Seq.map (fun number -> number * (number + 1) / 2) |> HashSet<int> :> ISet<int>
        names |> Seq.map calcWordValue |> Seq.filter (fun number -> triangleNumbers.Contains(number)) |> Seq.length

    [<TestCase("problem_042.dat", 162, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)
