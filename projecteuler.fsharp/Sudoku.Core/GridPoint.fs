namespace Sudoku.Core

open System;

type public GridPoint =
    struct
        val Row: int
        val Column: int
        new(row: int, column: int) =
            if row < 1 then raise(ArgumentOutOfRangeException("row"))
            if column < 1 then raise(ArgumentOutOfRangeException("column"))
            {Row = row; Column = column}
    end
