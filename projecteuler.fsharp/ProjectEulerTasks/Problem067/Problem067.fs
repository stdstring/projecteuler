namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open CommonLib

[<TestFixture>]
type Problem067() =

    let solveImpl (dataFilename: string) =
        let grid = File.ReadAllLines(Path.Combine("Data", dataFilename)) |> Seq.map (fun line -> line.Split(' ') |> Seq.map int |> Seq.toArray) |> Seq.toArray
        let rowMax = grid.Length
        let columnMax = grid.[rowMax - 1].Length
        let  generateNextPoints (point: GridPoint) =
            match point with
            | _ when (point.Row = rowMax) -> []
            | _ -> [GridPoint(point.Row + 1, point.Column); GridPoint(point.Row + 1, point.Column + 1)]
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = leftValue - rightValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let result = pathFactory.Create(grid, [GridPoint(1, 1)], [ for column in 1 .. columnMax -> GridPoint(rowMax, column) ])
        result.Value

    [<TestCase("problem_067.dat", 7273, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)

