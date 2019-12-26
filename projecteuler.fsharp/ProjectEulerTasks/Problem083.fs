namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO
open CommonLib

// In the 5 by 5 matrix (more see here: https://projecteuler.net/problem=83), the minimal path sum from the top left to the bottom right, by moving left, right, up, and down, is equal to 2297.
// Find the minimal path sum, in problem_083.dat, a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by moving left, right, up, and down.

[<TestFixture>]
type Problem083() =

    let solveImpl (dataFilename: string) =
        let grid = File.ReadAllLines(Path.Combine("Data", dataFilename)) |> Seq.map (fun line -> line.Split(',') |> Seq.map int |> Seq.toArray) |> Seq.toArray
        let rowMax = grid.Length
        let columnMax = grid.[0].Length
        let  generateNextPoints (point: GridPoint) =
            match point with
            | _ when (point.Row = rowMax) && (point.Column = columnMax) -> []
            | _ when (point.Row = 1) && (point.Column = 1) -> [GridPoint(1, 2); GridPoint(2, 1)]
            | _ when (point.Row = 1) && (point.Column = columnMax) -> [GridPoint(2, columnMax); GridPoint(1, columnMax - 1)]
            | _ when (point.Row = rowMax) && (point.Column = 1) -> [GridPoint(rowMax - 1, 1); GridPoint(rowMax, 2)]
            | _ when (point.Row = 1) -> [GridPoint(2, point.Column); GridPoint(1, point.Column - 1); GridPoint(1, point.Column + 1)]
            | _ when (point.Row = rowMax) -> [GridPoint(rowMax - 1, point.Column); GridPoint(rowMax, point.Column - 1); GridPoint(rowMax, point.Column + 1)]
            | _ when (point.Column = 1) -> [GridPoint(point.Row - 1, 1); GridPoint(point.Row + 1, 1); GridPoint(point.Row, 2)]
            | _ when (point.Column = columnMax) -> [GridPoint(point.Row - 1, columnMax); GridPoint(point.Row + 1, columnMax); GridPoint(point.Row, columnMax - 1)]
            | _ -> [GridPoint(point.Row - 1, point.Column); GridPoint(point.Row + 1, point.Column); GridPoint(point.Row, point.Column - 1); GridPoint(point.Row, point.Column + 1)]
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = rightValue - leftValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let result = pathFactory.Create(grid, [GridPoint(1, 1)], [GridPoint(rowMax, columnMax)])
        result.Value

    [<TestCase("problem_083.dat", 425185, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)