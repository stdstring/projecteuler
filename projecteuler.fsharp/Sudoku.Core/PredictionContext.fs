namespace Sudoku.Core

open System.Collections.Generic;

module PredictionContext =
    type Context = {freeCells: Queue<GridPoint>; number: int; grid: Grid}

    let findPredictObject (grid: Grid) =
        let foldFun (result: option<CellsInfo.CellsInfo>) (cellsInfo: CellsInfo.CellsInfo) =
            match result with
            | None -> Some cellsInfo
            | Some value when value.data.Count <= cellsInfo.data.Count -> Some value
            | _ -> Some cellsInfo
        let rowPredictObject = seq { 1 .. Common.GridSide }
                               |> Seq.map (fun row -> row |> CellsInfo.scanRow grid)
                               |> Seq.filter (fun info -> not (info.data |> Map.isEmpty))
                               |> Seq.fold foldFun None
        let columnPredictObject = seq { 1 .. Common.GridSide }
                                  |> Seq.map (fun column -> column |> CellsInfo.scanColumn grid)
                                  |> Seq.filter (fun info -> not (info.data |> Map.isEmpty))
                                  |> Seq.fold foldFun rowPredictObject
        let squarePredictObject = Common.squares
                                  |> Seq.map (fun square -> square |> CellsInfo.scanSquare grid)
                                  |> Seq.filter (fun info -> not (info.data |> Map.isEmpty))
                                  |> Seq.fold foldFun columnPredictObject
        match squarePredictObject with
        | None -> failwith ("Bad match")
        | Some cellsInfo -> cellsInfo

    let create (grid: Grid) =
        let predictObject = grid |> findPredictObject
        let _, predictObjectFirstNumbersBinary = predictObject.data |> Map.toSeq |> Seq.head
        let number, numbersBinary = seq { Common.MinNumber .. Common.MaxNumber }
                                    |> Seq.map (fun number -> number, number |> NumbersBinary.createForNumber)
                                    |> Seq.find (fun (number, numbersBinary) -> (numbersBinary &&& predictObjectFirstNumbersBinary) <> NumbersBinary.zero)
        let cells = predictObject.data
                    |> Map.map (fun cell cellNumbersBinary -> cellNumbersBinary |> NumbersBinary.appendCell grid cell)
                    |> Map.filter (fun _ sourceNumbersBinary -> (numbersBinary &&& sourceNumbersBinary) <> NumbersBinary.zero)
                    |> Map.toSeq
                    |> Seq.map (fun (cell, _) -> cell)
        {Context.freeCells = new Queue<GridPoint>(cells); Context.number = number; Context.grid = grid}
