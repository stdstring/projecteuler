namespace CommonLib

type GridPoint =
    struct
        val Row: int
        val Column: int
        new(row: int, column: int) = { Row = row; Column = column }
    end

type IGridPathBehavior<'TValue> =

    abstract member GenerateNextValue: accValue: 'TValue * pointValue: 'TValue->'TValue
    abstract member CompareValues: left: 'TValue * right: 'TValue->int
    abstract member GenerateNextPoint: currentPoint: GridPoint->GridPoint list

type GridPathResult<'TValue> =
    struct
        val Value: 'TValue
        val Path: GridPoint list
        new(value: 'TValue, path: GridPoint list) = { Value = value; Path = path }
    end

module GridPathImpl =

    type GridPathItem<'TValue> = {Value: 'TValue; Path: GridPoint list}

    let inline getGridValue (grid: 'TValue[][]) (point: GridPoint) =
        grid.[point.Row - 1].[point.Column - 1]

    let inline getGridDataValue (gridData: GridPathItem<'TValue> option[][]) (point: GridPoint) =
        gridData.[point.Row - 1].[point.Column - 1]

    let inline setGridDataValue (gridData: GridPathItem<'TValue> option[][]) (point: GridPoint) (value: GridPathItem<'TValue>) =
        gridData.[point.Row - 1].[point.Column - 1]<-value |> Some

open GridPathImpl

type GridPathFactory<'TValue>(behavior: IGridPathBehavior<'TValue>) =

    let behavior = behavior

    let createGridData (grid: 'TValue[][]) (initPoints: GridPoint list) =
        let gridData = grid |> Seq.map (fun row -> Array.create row.Length None) |> Seq.toArray
        initPoints |> Seq.iter (fun point -> {GridPathItem.Value = point |> getGridValue grid; GridPathItem.Path = [point]} |> setGridDataValue gridData point)
        gridData

    let selectBestResult (gridData: GridPathItem<'TValue> option[][]) (resultPoints: GridPoint list) =
        let firstResultPoint = resultPoints |> Seq.head
        let foldFun (bestItem: GridPathItem<'TValue>) (point: GridPoint) =
            let currentItem = (point |> getGridDataValue gridData).Value
            match behavior.CompareValues(bestItem.Value, currentItem.Value) with
            | value when value <= 0 -> currentItem
            | _ -> bestItem
        resultPoints |> Seq.tail |> Seq.fold foldFun (firstResultPoint |> getGridDataValue gridData).Value

    let processNextPoint (grid: 'TValue[][]) (gridData: GridPathItem<'TValue> option[][]) (currentData: GridPathItem<'TValue>) (nextPoint: GridPoint) (dest: GridPoint list) =
        let nextValue = behavior.GenerateNextValue(currentData.Value, nextPoint |> getGridValue grid)
        match nextPoint |> getGridDataValue gridData with
        | None ->
            {GridPathItem.Value = nextValue; GridPathItem.Path = nextPoint :: currentData.Path} |> setGridDataValue gridData nextPoint
            nextPoint :: dest
        | Some nextData ->
            match behavior.CompareValues(nextData.Value, nextValue) with
            | value when value <= 0 ->
                {GridPathItem.Value = nextValue; GridPathItem.Path = nextPoint :: currentData.Path} |> setGridDataValue gridData nextPoint
                nextPoint :: dest
            | _ -> dest

    let rec processNextPoints (grid: 'TValue[][]) (gridData: GridPathItem<'TValue> option[][]) (currentPoint: GridPoint) (nextPoints: GridPoint list) (destPoints: GridPoint list) =
        let currentData = (currentPoint |> getGridDataValue gridData).Value
        match nextPoints with
        | [] -> destPoints
        | nextPoint :: nextPointsRest -> destPoints |> processNextPoint grid gridData currentData nextPoint |> processNextPoints grid gridData currentPoint nextPointsRest

    let rec processPoint (grid: 'TValue[][]) (gridData: GridPathItem<'TValue> option[][]) (points: GridPoint list) (destPoints: GridPoint list) =
        match points with
        | [] -> destPoints
        | point :: pointsRest -> destPoints |> processNextPoints grid gridData point (point |> behavior.GenerateNextPoint) |> processPoint grid gridData pointsRest

    let rec processPoints (grid: 'TValue[][]) (gridData: GridPathItem<'TValue> option[][]) (points: GridPoint list) =
        match points with
        | [] -> ()
        | _ -> [] |> processPoint grid gridData points |> processPoints grid gridData

    member public this.Create(grid: 'TValue[][], initPoints: GridPoint list, resultPoints: GridPoint list) =
        let gridData = createGridData grid initPoints
        initPoints |> processPoints grid gridData
        let bestResult = selectBestResult gridData resultPoints
        GridPathResult<'TValue>(bestResult.Value, bestResult.Path |> List.rev)
