namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open Sudoku.Core;
open System;
open System.IO

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
