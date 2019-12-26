namespace Sudoku.Core.Tests

open System;
open System.Text;
open Sudoku.Core;

module GridConversion =
    let convertStringToGrid (source: string) =
        let dest = new Grid(Common.GridSide, Common.GridSide);
        source |> Seq.map (fun ch -> ch |> Char.GetNumericValue |> int) |> Seq.iteri (fun index number -> dest.[1 + index / Common.GridSide, 1 + index % Common.GridSide] <- number)
        dest

    let convertGridToString (grid: Grid) =
        let dest = new StringBuilder()
        seq { for row in 1 .. Common.GridSide do
                  for column in 1 .. Common.GridSide do
                      yield GridPoint(row, column) } |> Seq.iter (fun point -> dest.Append(grid.[point]) |> ignore)
        dest.ToString()
