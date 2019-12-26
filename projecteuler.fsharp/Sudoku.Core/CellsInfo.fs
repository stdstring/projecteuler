namespace Sudoku.Core

module CellsInfo =
    type CellsInfo = {data: Map<GridPoint, NumbersBinary.NumbersBinary>}

    let scanCells (grid: Grid) (cells: seq<GridPoint>) =
        let allNumbers = NumbersBinary.createForRange Common.MinNumber Common.MaxNumber
        let numbers = cells |> Seq.map (fun cell -> grid.[cell]) |> Seq.filter (fun value -> value <> 0) |> Seq.fold (fun result value -> result |> NumbersBinary.useNumber value) allNumbers
        let foldFun (cellsInfo: CellsInfo) (point: GridPoint) =
            match grid.[point] with
            | 0 -> {CellsInfo.data = cellsInfo.data |> Map.add point numbers}
            | _ -> cellsInfo
        cells |> Seq.fold foldFun {CellsInfo.data = Map.empty}

    let scanRow (grid: Grid) (row: int) =
        row |> Common.generateRowPoints |> scanCells grid

    let scanColumn (grid: Grid) (column: int) =
        column |> Common.generateColumnPoints |> scanCells grid

    let scanSquare (grid: Grid) (point: GridPoint) =
        point |> Common.generateSquarePoints |> scanCells grid

    let createForRow (grid: Grid) (row: int) =
        let cellsInfo = row |> scanRow grid
        let foldFun (data: Map<GridPoint, NumbersBinary.NumbersBinary>) (cell: GridPoint) (sourceValue: NumbersBinary.NumbersBinary) =
            let destValue = sourceValue |> NumbersBinary.appendColumn grid cell.Column |> NumbersBinary.appendSquare grid cell
            data |> Map.add cell destValue
        {CellsInfo.data = cellsInfo.data |> Map.fold foldFun cellsInfo.data}

    let createForColumn (grid: Grid) (column: int) =
        let cellsInfo = column |> scanColumn grid
        let foldFun (data: Map<GridPoint, NumbersBinary.NumbersBinary>) (cell: GridPoint) (sourceValue: NumbersBinary.NumbersBinary) =
            let destValue = sourceValue |> NumbersBinary.appendRow grid cell.Row |> NumbersBinary.appendSquare grid cell
            data |> Map.add cell destValue
        {CellsInfo.data = cellsInfo.data |> Map.fold foldFun cellsInfo.data}

    let createForSquare (grid: Grid) (point: GridPoint) =
        let cellsInfo = point |> scanSquare grid
        let foldFun (data: Map<GridPoint, NumbersBinary.NumbersBinary>) (cell: GridPoint) (sourceValue: NumbersBinary.NumbersBinary) =
            let destValue = sourceValue |> NumbersBinary.appendRow grid cell.Row |> NumbersBinary.appendColumn grid cell.Column
            data |> Map.add cell destValue
        {CellsInfo.data = cellsInfo.data |> Map.fold foldFun cellsInfo.data}
