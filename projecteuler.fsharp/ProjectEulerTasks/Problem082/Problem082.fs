namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open CommonLib

[<TestFixture>]
type Problem082() =

    let solveImpl (dataFilename: string) =
        let grid = File.ReadAllLines(Path.Combine("Data", dataFilename)) |> Seq.map (fun line -> line.Split(',') |> Seq.map int |> Seq.toArray) |> Seq.toArray
        let rowMax = grid.Length
        let columnMax = grid.[0].Length
        let  generateNextPoints (point: GridPoint) =
            match point with
            | _ when (point.Column = columnMax) -> []
            | _ when (point.Row = 1) -> [GridPoint(1, point.Column + 1); GridPoint(2, point.Column)]
            | _ when (point.Row = rowMax) -> [GridPoint(rowMax, point.Column + 1); GridPoint(rowMax - 1, point.Column)]
            | _ -> [GridPoint(point.Row + 1, point.Column); GridPoint(point.Row - 1, point.Column); GridPoint(point.Row, point.Column + 1)]
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = rightValue - leftValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let initPoints = [ for row in 1 .. rowMax -> GridPoint(row, 1) ]
        let resultPoints = [ for row in 1 .. rowMax -> GridPoint(row, columnMax) ]
        let result = pathFactory.Create(grid, initPoints, resultPoints)
        result.Value

    [<TestCase("problem_082.dat", 260324, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)
