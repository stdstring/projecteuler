namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

// What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20*20 grid (situated in "problem_011.dat")?

[<TestFixture>]
type Problem011() =

    let solveImpl (dataFilename: string) (count: int) =
        let grid = File.ReadAllLines(Path.Combine("Data", dataFilename)) |> Array.map (fun line -> line.Split(' ') |> Array.map(fun value -> value |> int))
        let rowCount = grid.Length
        let columnCount = grid.[0].Length
        let getGridValue (row: int) (column: int) =
            match row, column with
            | _, _ when (row < 0) || (row >= rowCount) || (column < 0) || (column >= columnCount) -> 0
            | _, _ -> grid.[row].[column]
        let calcProduct (row: int) (column: int) (rowDelta: int) (columnDelta: int) =
            seq {0 .. count - 1} |> Seq.fold (fun product index -> product * getGridValue (row + index * rowDelta) (column  + index * columnDelta)) 1
        seq {for row in 0 .. rowCount - 1 do for column in 0 .. columnCount do yield (row, column)} |>
        Seq.map (fun (row, column) -> [calcProduct row column 1 0; calcProduct row column 0 1; calcProduct row column 1 1; calcProduct row column -1 1] |> List.max) |>
        Seq.max

    [<TestCase("problem_011.dat", 4, 70600674, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, count: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl dataFilename count)