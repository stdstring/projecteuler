namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// Consider the problem of building a wall out of 2*1 and 3*1 bricks (horizontal*vertical dimensions) such that,
// for extra strength, the gaps between horizontally-adjacent bricks never line up in consecutive layers, i.e. never form a "running crack".
// See example of non acceptable wall here: https://projecteuler.net/problem=215
// There are eight ways of forming a crack-free 9*3 wall, written W(9,3) = 8.
// Calculate W(32,10).

[<TestFixture>]
type Problem215() =

    [<Literal>]
    let SmallBrickSize = 2

    [<Literal>]
    let LargeBrickSize = 3

    let copyAndFill (fillValue: int) (rowTemplate: int[]) =
        let row = rowTemplate |> Array.copy
        for index in 0 .. row.Length - 1 do
            if row.[index] = 0 then
                row.[index] <- fillValue
        row

    let setRowValue (index: int) (value: int) (row: int[]) =
        row.[index] <- value
        row

    let saveRow (fillValue: int) (storage: ResizeArray<int[]>) (row: int[]) =
        row |> copyAndFill fillValue |> storage.Add
        row

    let generateCombinations (smallBricksCount: int) (largeBricksCount: int) (combinations: ResizeArray<int[]>) =
        let rowSize = smallBricksCount + largeBricksCount
        let generateValue = if smallBricksCount < largeBricksCount then SmallBrickSize else LargeBrickSize
        let generateValueCount = if smallBricksCount < largeBricksCount then smallBricksCount else largeBricksCount
        let fillValue = if smallBricksCount < largeBricksCount then LargeBrickSize else SmallBrickSize
        let rec generateImpl (valueNumber: int) (fromIndex: int) (rowTemplate: int[]) =
            let numberDelta = generateValueCount - valueNumber
            match numberDelta with
            | 0 ->
                seq {fromIndex .. rowSize - 1} |> Seq.iter (fun index -> rowTemplate |> setRowValue index generateValue |> saveRow fillValue combinations |> setRowValue index 0 |> ignore)
                rowTemplate
            | _ ->
                seq {fromIndex .. rowSize - numberDelta} |> Seq.iter (fun index -> rowTemplate |> setRowValue index generateValue |> generateImpl (valueNumber + 1) (index + 1) |> setRowValue index 0 |> ignore)
                rowTemplate
        rowSize |> Array.zeroCreate |> generateImpl 1 0 |> ignore

    let generateAllCombinations (width: int) =
        let combinations = new ResizeArray<int[]>()
        for smallCount in 0 .. width / SmallBrickSize do
            if (width - smallCount * SmallBrickSize) % LargeBrickSize = 0 then
                let largeCount = (width - smallCount * SmallBrickSize) / LargeBrickSize
                if smallCount = 0 then
                    Array.create largeCount LargeBrickSize |> combinations.Add
                elif largeCount = 0 then
                    Array.create smallCount SmallBrickSize |> combinations.Add
                else
                    combinations |> generateCombinations smallCount largeCount
        combinations

    let isRowsAcceptable (row1: int[]) (row2: int[]) =
        let rec isRowAcceptableImpl (index1: int) (index2: int) (length1: int) (length2: int) =
            match index1 < row1.Length && index2 < row2.Length with
            | false -> true
            | true ->
                match length1 with
                | _ when length1 = length2 -> false
                | _ when length1 < length2 -> isRowAcceptableImpl (index1 + 1) index2 (length1 + row1.[index1]) length2
                | _ when length1 > length2 -> isRowAcceptableImpl index1 (index2 + 1) length1 (length2 + row2.[index2])
                | _ -> failwith "Unexpected branch of match expression"
        isRowAcceptableImpl 1 1 row1.[0] row2.[0]

    let generateRowsAcceptableTable (combinations: ResizeArray<int[]>) =
        let rowsAcceptableTable = Array2D.zeroCreate combinations.Count combinations.Count
        for first in 0 .. combinations.Count - 1 do
            for second in first + 1 .. combinations.Count - 1 do
                let firstRow = combinations.[first]
                let secondRow = combinations.[second]
                match first with
                | _ when firstRow.[0] = secondRow.[0] -> ()
                | _ when firstRow.[firstRow.Length - 1] = secondRow.[secondRow.Length - 1] -> ()
                | _ when isRowsAcceptable firstRow secondRow ->
                    rowsAcceptableTable.[first, second] <- true
                    rowsAcceptableTable.[second, first] <- true
                | _ -> ()
        rowsAcceptableTable

    let rec processBricksLine (combinationsCount: int) (rowsAcceptableTable: bool[,]) (height: int) (lineNumber: int) (rowData: int64[]) =
        match lineNumber = height with
        | true -> rowData
        | false ->
            let nextRowData = combinationsCount |> Array.zeroCreate
            for first in 0 .. combinationsCount - 1 do
                for second in first + 1 .. combinationsCount - 1 do
                    if rowsAcceptableTable.[first, second] then
                        nextRowData.[second] <- nextRowData.[second] + rowData.[first]
                        nextRowData.[first] <- nextRowData.[first] + rowData.[second]
            nextRowData |> processBricksLine combinationsCount rowsAcceptableTable height (lineNumber + 1)

    let solveImpl (width: int) (height: int) =
        let combinations = generateAllCombinations width
        let rowsAcceptableTable = generateRowsAcceptableTable combinations
        1L |> Array.create combinations.Count |> processBricksLine combinations.Count rowsAcceptableTable height 1 |> Seq.sum

    [<TestCase(9, 3, 8L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(32, 10, 806844323190414L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(width: int, height: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, width, height)