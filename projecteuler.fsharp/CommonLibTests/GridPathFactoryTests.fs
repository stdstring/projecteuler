namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy

[<TestFixture>]
type GridPathFactoryTests() =

    let squareGrid = [|[|131; 673; 234; 103; 18|]; [|201; 96; 342; 965; 150|]; [|630; 803; 746; 422; 111|]; [|537; 699; 497; 121; 956|]; [|805; 732; 524; 37; 331|]|]

    let squareRowMax = 5

    let squareColumnMax = 5

    let generateTwoWayNextPoints (point: GridPoint) =
        match point with
        | _ when (point.Row = squareRowMax) && (point.Column = squareColumnMax) -> []
        | _ when (point.Row = squareRowMax) -> [GridPoint(squareRowMax, point.Column + 1)]
        | _ when (point.Column = squareColumnMax) -> [GridPoint(point.Row + 1, squareColumnMax)]
        | _ -> [GridPoint(point.Row + 1, point.Column); GridPoint(point.Row, point.Column + 1)]

    let generateThreeWayNextPoints (point: GridPoint) =
        match point with
        | _ when (point.Column = squareColumnMax) -> []
        | _ when (point.Row = 1) -> [GridPoint(1, point.Column + 1); GridPoint(2, point.Column)]
        | _ when (point.Row = squareRowMax) -> [GridPoint(squareRowMax, point.Column + 1); GridPoint(squareRowMax - 1, point.Column)]
        | _ -> [GridPoint(point.Row + 1, point.Column); GridPoint(point.Row - 1, point.Column); GridPoint(point.Row, point.Column + 1)]

    let generateFourWayNextPoints (point: GridPoint) =
        match point with
        | _ when (point.Row = squareRowMax) && (point.Column = squareColumnMax) -> []
        | _ when (point.Row = 1) && (point.Column = 1) -> [GridPoint(1, 2); GridPoint(2, 1)]
        | _ when (point.Row = 1) && (point.Column = squareColumnMax) -> [GridPoint(2, squareColumnMax); GridPoint(1, squareColumnMax - 1)]
        | _ when (point.Row = squareRowMax) && (point.Column = 1) -> [GridPoint(squareRowMax - 1, 1); GridPoint(squareRowMax, 2)]
        | _ when (point.Row = 1) -> [GridPoint(2, point.Column); GridPoint(1, point.Column - 1); GridPoint(1, point.Column + 1)]
        | _ when (point.Row = squareRowMax) -> [GridPoint(squareRowMax - 1, point.Column); GridPoint(squareRowMax, point.Column - 1); GridPoint(squareRowMax, point.Column + 1)]
        | _ when (point.Column = 1) -> [GridPoint(point.Row - 1, 1); GridPoint(point.Row + 1, 1); GridPoint(point.Row, 2)]
        | _ when (point.Column = squareColumnMax) -> [GridPoint(point.Row - 1, squareColumnMax); GridPoint(point.Row + 1, squareColumnMax); GridPoint(point.Row, squareColumnMax - 1)]
        | _ -> [GridPoint(point.Row - 1, point.Column); GridPoint(point.Row + 1, point.Column); GridPoint(point.Row, point.Column - 1); GridPoint(point.Row, point.Column + 1)]

    let triangleGrid = [|[|3|]; [|7; 4|]; [|2; 4; 6|]; [|8; 5; 9; 3|]|]

    let triangleRowMax = 4

    let triangleColumnMax = 4

    let generateDirectTriangleNextPoints (point: GridPoint) =
        match point with
        | _ when (point.Row = triangleRowMax) -> []
        | _ -> [GridPoint(point.Row + 1, point.Column); GridPoint(point.Row + 1, point.Column + 1)]

    let generateReverseTriangleNextPoints (point: GridPoint) =
        match point with
        | _ when (point.Row = 1) && (point.Column = 1) -> []
        | _ when (point.Column = 1) -> [GridPoint(point.Row - 1, 1)]
        | _ when (point.Row = point.Column) -> [GridPoint(point.Row - 1, point.Column - 1)]
        | _ -> [GridPoint(point.Row - 1, point.Column - 1); GridPoint(point.Row - 1, point.Column)]

    [<Test>]
    member public this.SearchTwoWayTest() =
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = rightValue - leftValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateTwoWayNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let result = pathFactory.Create(squareGrid, [GridPoint(1, 1)], [GridPoint(squareRowMax, squareColumnMax)])
        ClassicAssert.AreEqual(2427, result.Value)
        let expectedPath = [(1, 1); (2, 1); (2, 2); (2, 3); (3, 3); (3, 4); (4, 4); (5, 4); (5, 5)] |> List.map GridPoint
        ClassicAssert.AreEqual(expectedPath, result.Path)

    [<Test>]
    member public this.SearchThreeWayTest() =
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = rightValue - leftValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateThreeWayNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let initPoints = [ for row in 1 .. squareRowMax -> GridPoint(row, 1) ]
        let resultPoints = [ for row in 1 .. squareRowMax -> GridPoint(row, squareColumnMax) ]
        let result = pathFactory.Create(squareGrid, initPoints, resultPoints)
        ClassicAssert.AreEqual(994, result.Value)
        let expectedPath = [(2, 1); (2, 2); (2, 3); (1, 3); (1, 4); (1, 5)] |> List.map GridPoint
        ClassicAssert.AreEqual(expectedPath, result.Path)

    [<Test>]
    member public this.SearchFourWayTest() =
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = rightValue - leftValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateFourWayNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let result = pathFactory.Create(squareGrid, [GridPoint(1, 1)], [GridPoint(squareRowMax, squareColumnMax)])
        ClassicAssert.AreEqual(2297, result.Value)
        let expectedPath = [(1, 1); (2, 1); (2, 2); (2, 3); (1, 3); (1, 4); (1, 5); (2, 5); (3, 5); (3, 4); (4, 4); (5, 4); (5, 5)] |> List.map GridPoint
        ClassicAssert.AreEqual(expectedPath, result.Path)

    [<Test>]
    member public this.SearchDirectTriangleTest() =
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = leftValue - rightValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateDirectTriangleNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let result = pathFactory.Create(triangleGrid, [GridPoint(1, 1)], [ for column in 1 .. triangleColumnMax -> GridPoint(triangleRowMax, column) ])
        ClassicAssert.AreEqual(23, result.Value)
        ClassicAssert.AreEqual([(1, 1); (2, 1); (3, 2); (4, 3)] |> List.map GridPoint, result.Path)

    [<Test>]
    member public this.SearchReverseTriangleTest() =
        let behavior = { new IGridPathBehavior<int> with
                             member this.GenerateNextValue(accValue: int, pointValue: int) = accValue + pointValue
                             member this.CompareValues(leftValue: int, rightValue: int) = leftValue - rightValue
                             member this.GenerateNextPoint (point: GridPoint) = point |> generateReverseTriangleNextPoints
                       }
        let pathFactory = GridPathFactory<int>(behavior)
        let result = pathFactory.Create(triangleGrid, [ for column in 1 .. triangleColumnMax -> GridPoint(triangleRowMax, column) ], [GridPoint(1, 1)])
        ClassicAssert.AreEqual(23, result.Value)
        ClassicAssert.AreEqual([(4, 3); (3, 2); (2, 1); (1, 1)] |> List.map GridPoint, result.Path)
