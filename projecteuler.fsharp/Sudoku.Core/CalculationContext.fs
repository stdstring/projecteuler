namespace Sudoku.Core

module CalculationContext =
    type Context = {grid: Grid; emptyCount: int}

    let create (grid: Grid) =
        let emptyCount = seq { for row in 1 .. grid.RowCount do
                                   for column in 1 .. grid.ColumnCount do
                                       yield GridPoint(row, column) } |> Seq.filter (fun cell -> grid.[cell] = 0) |> Seq.length
        {Context.grid = grid.Clone(); Context.emptyCount = emptyCount}
