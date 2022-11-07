namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem116() =

    [<Literal>]
    let RedTileSize = 2

    [<Literal>]
    let GreenTileSize = 3

    [<Literal>]
    let BlueTileSize = 4

    let generateWaysCountData (totalSize: int) (tileSize: int) =
        let storage = Array.zeroCreate (totalSize + 1)
        storage.[0] <- 1L
        for size in seq {1 .. totalSize} do
            // grey square
            storage.[size] <- storage.[size - 1]
            // tile
            if size >= tileSize then
                storage.[size] <- storage.[size] + storage.[size - tileSize]
        storage

    let solveImpl (totalSize: int) =
        let redTileStorage = RedTileSize |> generateWaysCountData totalSize
        let greenTileStorage = GreenTileSize |> generateWaysCountData totalSize
        let blueTileStorage = BlueTileSize |> generateWaysCountData totalSize
        // remove case which consists of only gray squares
        (redTileStorage.[totalSize] - 1L) + (greenTileStorage.[totalSize] - 1L) + (blueTileStorage.[totalSize] - 1L)


    [<TestCase(5, 12L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(50, 20492570929L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(totalSize: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, totalSize)
