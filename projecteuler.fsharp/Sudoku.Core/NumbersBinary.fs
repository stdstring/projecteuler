namespace Sudoku.Core

open System;

module NumbersBinary =
    type NumbersBinary =
        {value: int}

        static member (&&&) (left: NumbersBinary, right: NumbersBinary) =
            {NumbersBinary.value = left.value &&& right.value}

        static member (|||) (left: NumbersBinary, right: NumbersBinary) =
            {NumbersBinary.value = left.value ||| right.value}

    let zero = {NumbersBinary.value = 0}

    let createForNumber (number: int) =
        {NumbersBinary.value = 1 <<< (number - 1)}

    let createForRange (minNumber: int) (maxNumber: int) =
        seq { for number in minNumber .. maxNumber -> createForNumber(number) } |> Seq.fold (fun result value -> result ||| value) zero

    let useNumber (number: int) (numbers: NumbersBinary) =
        {NumbersBinary.value = numbers.value &&& ~~~(1 <<< (number - 1))}

    let appendCells (numbers: NumbersBinary) (grid: Grid) (cells: seq<GridPoint>) =
        cells |> Seq.map (fun cell -> grid.[cell]) |> Seq.filter (fun value -> value <> 0) |> Seq.fold (fun result value -> result |> useNumber value) numbers

    let appendRow (grid: Grid) (row: int) (numbers: NumbersBinary) =
        row |> Common.generateRowPoints |> appendCells numbers grid

    let appendColumn (grid: Grid) (column: int) (numbers: NumbersBinary) =
        column |> Common.generateColumnPoints |> appendCells numbers grid

    let appendSquare (grid: Grid) (point: GridPoint) (numbers: NumbersBinary) =
        point |> Common.generateSquarePoints |> appendCells numbers grid

    let appendCell (grid: Grid) (cell: GridPoint) (numbers: NumbersBinary) =
        numbers |> appendRow grid cell.Row |> appendColumn grid cell.Column |> appendSquare grid cell
