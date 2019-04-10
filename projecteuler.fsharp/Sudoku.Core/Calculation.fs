namespace Sudoku.Core

module Calculation =
    type CalculationResult =
        | Finish of CalculationContext.Context
        | NeedPrediction of CalculationContext.Context
        | Stop

    type CalculationContinuation =
        | Continue of CalculationContext.Context
        | Finish of CalculationContext.Context
        | Stop

    let chooseCellsInfoData (cellsInfo: CellsInfo.CellsInfo) =
        let data = seq { for cell, numbersBinary in cellsInfo.data |> Map.toSeq do
                             for number in Common.MinNumber .. Common.MaxNumber do
                                 yield cell, numbersBinary, number }
                   |> Seq.filter (fun (_, numbersBinary, number) -> numbersBinary = (number |> NumbersBinary.createForNumber))
        match data |> Seq.isEmpty with
        | true -> None
        | false ->
            let cell, _, number = data |> Seq.head
            Some (cell, number)

    let chooseNumbersInfoData (numbersInfo: NumbersInfo.NumbersInfo) =
        let filterFun (key: int) (value: list<GridPoint>) =
            match value with
            | [_] -> true
            | _ -> false
        let data = numbersInfo.data |> Map.filter filterFun |> Map.toSeq
        match data |> Seq.isEmpty with
        | true -> None
        | false ->
            match data |> Seq.head with
            | number, [cell] -> Some (cell, number)
            | _ -> failwith "Bad data"

    let chooseCell (cellsInfo: CellsInfo.CellsInfo) (numbersInfo: NumbersInfo.NumbersInfo) =
        match cellsInfo |> chooseCellsInfoData with
        | None -> numbersInfo |> chooseNumbersInfoData
        | other -> other

    let useCellsInfoData (point: GridPoint) (number: int) (cellsInfo: CellsInfo.CellsInfo) =
        {CellsInfo.CellsInfo.data = cellsInfo.data |> Map.remove point |> Map.map (fun _ source -> source |> NumbersBinary.useNumber number)}

    let useNumbersInfoData (point: GridPoint) (number: int) (numbersInfo: NumbersInfo.NumbersInfo) =
        {NumbersInfo.NumbersInfo.data = numbersInfo.data |> Map.remove number |> Map.map (fun _ cells -> cells |> List.filter (fun cell -> cell <> point))}

    let checkCalculation (cellsInfo: CellsInfo.CellsInfo) (numbersInfo: NumbersInfo.NumbersInfo) =
        let cellsInfoResult = cellsInfo.data |> Map.forall (fun _ numbersBinary -> numbersBinary <> NumbersBinary.zero)
        let numbersInfoResult = numbersInfo.data |> Map.forall (fun _ cells -> not (cells |> List.isEmpty))
        cellsInfoResult && numbersInfoResult

    let processCalculation (context: CalculationContext.Context) (cellsInfo: CellsInfo.CellsInfo) =
        let numbersInfo = cellsInfo |> NumbersInfo.create
        let rec processImpl (context: CalculationContext.Context) (cellsInfo: CellsInfo.CellsInfo) (numbersInfo: NumbersInfo.NumbersInfo) =
            match chooseCell cellsInfo numbersInfo with
            | None -> CalculationContinuation.Continue context
            | Some (cell, number) ->
                context.grid.[cell] <- number
                let newContext = {CalculationContext.grid = context.grid; CalculationContext.emptyCount = context.emptyCount - 1}
                match newContext.emptyCount with
                | 0 -> CalculationContinuation.Finish newContext
                | _ ->
                    let newCellsInfo = cellsInfo |> useCellsInfoData cell number
                    let newNumbersInfo = numbersInfo |> useNumbersInfoData cell number
                    match checkCalculation newCellsInfo newNumbersInfo with
                    | false -> CalculationContinuation.Stop
                    | true -> processImpl newContext newCellsInfo newNumbersInfo
        processImpl context cellsInfo numbersInfo

    let processRows (context: CalculationContext.Context) =
        let rec processRow (rows: seq<int>) (calculationContext: CalculationContext.Context) =
            match rows |> Seq.isEmpty with
            | true -> CalculationContinuation.Continue calculationContext
            | _ ->
                match CellsInfo.createForRow calculationContext.grid (rows |> Seq.head) |> processCalculation calculationContext with
                | CalculationContinuation.Stop -> CalculationContinuation.Stop
                | CalculationContinuation.Finish newCalculationContext -> CalculationContinuation.Finish newCalculationContext
                | CalculationContinuation.Continue newCalculationContext -> newCalculationContext |> processRow (rows |> Seq.tail)
        context |> processRow (seq { 1 .. Common.GridSide })


    let processColumns (context: CalculationContext.Context) =
        let rec processColumn (columns: seq<int>) (calculationContext: CalculationContext.Context) =
            match columns |> Seq.isEmpty with
            | true -> CalculationContinuation.Continue calculationContext
            | false ->
                match CellsInfo.createForColumn calculationContext.grid (columns |> Seq.head) |> processCalculation calculationContext with
                | CalculationContinuation.Stop -> CalculationContinuation.Stop
                | CalculationContinuation.Finish newCalculationContext -> CalculationContinuation.Finish newCalculationContext
                | CalculationContinuation.Continue newCalculationContext -> newCalculationContext |> processColumn (columns |> Seq.tail)
        context |> processColumn (seq { 1 .. Common.GridSide })

    let processSquares (context: CalculationContext.Context) =
        let rec processSquare (squares: list<GridPoint>) (calculationContext: CalculationContext.Context) =
            match squares with
            | [] -> CalculationContinuation.Continue calculationContext
            | point :: squaresRest ->
                match CellsInfo.createForSquare calculationContext.grid point |> processCalculation calculationContext with
                | CalculationContinuation.Stop -> CalculationContinuation.Stop
                | CalculationContinuation.Finish newCalculationContext -> CalculationContinuation.Finish newCalculationContext
                | CalculationContinuation.Continue newCalculationContext -> newCalculationContext |> processSquare squaresRest
        context |> processSquare Common.squares

    let calculate (context: CalculationContext.Context) =
        let rec calculateImpl (context: CalculationContext.Context) =
            let emptyCountBefore = context.emptyCount
            match context |> processRows with
            | CalculationContinuation.Stop -> CalculationResult.Stop
            | CalculationContinuation.Finish contextAfterRows -> CalculationResult.Finish contextAfterRows
            | CalculationContinuation.Continue contextAfterRows ->
                match contextAfterRows |> processColumns with
                | CalculationContinuation.Stop -> CalculationResult.Stop
                | CalculationContinuation.Finish contextAfterColumns -> CalculationResult.Finish contextAfterColumns
                | CalculationContinuation.Continue contextAfterColumns ->
                    match contextAfterColumns |> processSquares with
                    | CalculationContinuation.Stop -> CalculationResult.Stop
                    | CalculationContinuation.Finish contextAfterSquares -> CalculationResult.Finish contextAfterSquares
                    | CalculationContinuation.Continue contextAfterSquares ->
                        match emptyCountBefore with
                        | _ when contextAfterSquares.emptyCount = emptyCountBefore -> CalculationResult.NeedPrediction contextAfterSquares
                        | _ -> contextAfterSquares |> calculateImpl
        context |> calculateImpl
