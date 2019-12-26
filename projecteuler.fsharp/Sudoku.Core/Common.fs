namespace Sudoku.Core

module Common =
    [<Literal>]
    let SquareSide = 3

    [<Literal>]
    let GridSide = 9

    [<Literal>]
    let MinNumber = 1

    [<Literal>]
    let MaxNumber = 9

    let squares = [GridPoint(1, 1); GridPoint(1, 4); GridPoint(1, 7); GridPoint(4, 1); GridPoint(4, 4); GridPoint(4, 7); GridPoint(7, 1); GridPoint(7, 4); GridPoint(7, 7)]

    let generateRowPoints (row: int) =
        seq { for column in 1 .. GridSide -> GridPoint(row, column) }

    let generateColumnPoints (column: int) =
        seq { for row in 1 .. GridSide -> GridPoint(row, column) }

    let generateSquarePoints (point: GridPoint) =
        let topLeftRow = SquareSide * ((point.Row - 1) / SquareSide) + 1
        let topLeftColumn = SquareSide * ((point.Column - 1) / SquareSide) + 1
        seq { for row in 0 .. SquareSide - 1 do
                  for column in 0 .. SquareSide - 1 do
                      yield GridPoint(topLeftRow + row, topLeftColumn + column) }
