namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open System.Text

[<TestFixture>]
type Problem059() =

    let checkData = ["The "; " the "]

    let asciiEncoder = Encoding.ASCII
    let keyCharRangeStart = asciiEncoder.GetBytes("a").[0]
    let keyCharRangeEnd = asciiEncoder.GetBytes("z").[0]

    let repeatKey (size: int) (keyBasis: byte[]) =
        let key = Array.create size 0uy
        seq {0 .. size - 1} |> Seq.iter (fun index -> key.[index] <- keyBasis.[index % keyBasis.Length])
        key

    let decypt (source: byte[]) (key: byte[]) =
        seq {0 .. source.Length - 1} |> Seq.map (fun index -> source.[index] ^^^ key.[index]) |> Seq.toArray


    let solveImpl (dataFilename: string) =
        let source = File.ReadAllText(Path.Combine("Data", dataFilename)).Split(',') |> Seq.map (fun value -> value |> byte) |> Seq.toArray
        let result = seq {for key1 in keyCharRangeStart .. keyCharRangeEnd do
                          for key2 in keyCharRangeStart .. keyCharRangeEnd do
                          for key3 in keyCharRangeStart .. keyCharRangeEnd do yield [|key1; key2; key3|]} |>
                     Seq.map (fun keyBasis -> keyBasis |> repeatKey source.Length) |>
                     Seq.map (fun key -> key |> decypt source |> asciiEncoder.GetChars |> System.String) |>
                     Seq.filter (fun dest -> checkData |> List.exists dest.Contains) |> Seq.toList
        Assert.AreEqual(1, result.Length)
        result |> List.head |> asciiEncoder.GetBytes |> Seq.sumBy (fun value -> value |> int)

    [<TestCase("problem_059.dat", 107359, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)