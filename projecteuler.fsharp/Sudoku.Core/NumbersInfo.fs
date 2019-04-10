namespace Sudoku.Core

module NumbersInfo =
    type NumbersInfo = {data: Map<int, list<GridPoint>>}

    let create (cellsInfo: CellsInfo.CellsInfo) =
        let appendPoint (number: int) (cell: GridPoint) (data: Map<int, list<GridPoint>>) =
            data |> Map.add number (cell :: if Map.containsKey number data then data.[number] else [])
        let foldFun (data: Map<int, list<GridPoint>>) (cell: GridPoint) (sourceValue: NumbersBinary.NumbersBinary) =
            seq { Common.MinNumber .. Common.MaxNumber }
            |> Seq.filter (fun number -> (sourceValue &&& NumbersBinary.createForNumber(number)) <> NumbersBinary.zero)
            |> Seq.fold (fun result number -> result |> appendPoint number cell) data
        {NumbersInfo.data = cellsInfo.data |> Map.fold foldFun Map.empty}
