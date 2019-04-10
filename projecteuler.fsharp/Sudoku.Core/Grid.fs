namespace Sudoku.Core

open System;

type public Grid(rowCount: int, columnCount: int) =
    do
        if rowCount < 1 then raise(ArgumentOutOfRangeException("rowCount"))
        if columnCount < 1 then raise(ArgumentOutOfRangeException("columnCount"))
    let rowCount = rowCount
    let columnCount = columnCount
    let grid = Array.create (rowCount * columnCount) 0

    member public this.RowCount = rowCount
    member public this.ColumnCount = columnCount

    member public this.Item
        with get(row: int, column: int) =
            if (row < 1 || row > rowCount) then raise(ArgumentOutOfRangeException("row"))
            if (column < 1 || column > columnCount) then raise(ArgumentOutOfRangeException("column"))
            let index = (row - 1) * columnCount + (column - 1)
            grid.[index]
        and set(row: int, column: int) (value: int) =
            if (row < 1 || row > rowCount) then raise(ArgumentOutOfRangeException("row"))
            if (column < 1 || column > columnCount) then raise(ArgumentOutOfRangeException("column"))
            let index = (row - 1) * columnCount + (column - 1)
            grid.[index] <- value

    member public this.Item
        with get(point: GridPoint) =
            if (point.Row > rowCount) then raise(ArgumentOutOfRangeException("point"))
            if (point.Column > columnCount) then raise(ArgumentOutOfRangeException("point"))
            let index = (point.Row - 1) * columnCount + (point.Column - 1)
            grid.[index]
        and set(point: GridPoint) (value: int) =
            if (point.Row > rowCount) then raise(ArgumentOutOfRangeException("point"))
            if (point.Column > columnCount) then raise(ArgumentOutOfRangeException("point"))
            let index = (point.Row - 1) * columnCount + (point.Column - 1)
            grid.[index] <- value

    member public this.Clone() =
        let dest = new Grid(rowCount, columnCount)
        seq { for row in 1 .. rowCount do
                  for column in 1 .. columnCount do
                      yield GridPoint(row, column) } |> Seq.iter (fun point -> dest.[point] <- this.[point])
        dest