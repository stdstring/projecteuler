namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open Sudoku.Core;
open System;
open System.IO

// Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept.
// Its origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar, and much more difficult, puzzle idea called Latin Squares.
// The objective of Su Doku puzzles, however, is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column, and 3 by 3 box contains each of the digits 1 to 9.
// A well constructed Su Doku puzzle has a unique solution and can be solved by logic, although it may be necessary to employ "guess and test" methods in order to eliminate options (there is much contested opinion over this).
// The complexity of the search determines the difficulty of the puzzle.
// problem_096.dat is the data file, which contains fifty different Su Doku puzzles ranging in difficulty, but all with unique solutions.
// By solving all fifty puzzles find the sum of the 3-digit numbers found in the top left corner of each solution grid.

[<TestFixture>]
type Problem096() =

    let convertStringToGrid (source: string) =
        let dest = new Grid(Common.GridSide, Common.GridSide);
        source |> Seq.map (fun ch -> ch |> Char.GetNumericValue |> int) |> Seq.iteri (fun index number -> dest.[1 + index / Common.GridSide, 1 + index % Common.GridSide] <- number)
        dest

    let convertStringsToGrid (source: string[]) =
        source |> String.Concat |> convertStringToGrid

    let readPortion (start: int) (lines: string[]) =
        let name = lines.[start]
        let grid = seq { 1 .. Common.GridSide } |> Seq.map (fun index -> lines.[start + index]) |> Seq.toArray |> convertStringsToGrid
        name, grid

    let readInput (filename: string) =
        let lines = filename |> File.ReadAllLines
        seq { 0 .. 1 + Common.GridSide .. (lines |> Array.length) - 1} |> Seq.map (fun index -> lines |> readPortion index) |> Seq.toList

    let solveImpl (dataFilename: string) =
        let solver = new Solver()
        Path.Combine("Data", dataFilename) |> readInput |> List.map (fun (_, grid) -> grid |> solver.Solve) |> List.fold (fun result grid -> result + 100 * grid.[1, 1] + 10 * grid.[1, 2] + grid.[1, 3]) 0

    [<TestCase("problem_096.dat", 24702, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)
