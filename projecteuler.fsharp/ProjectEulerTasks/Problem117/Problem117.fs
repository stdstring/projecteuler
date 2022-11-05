namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem117() =

    [<Literal>]
    let RedTileSize = 2

    [<Literal>]
    let GreenTileSize = 3

    [<Literal>]
    let BlueTileSize = 4

    let generateWaysCountData (totalSize: int) (tiles: int list) =
        let storage = Array.zeroCreate (totalSize + 1)
        storage.[0] <- 1L
        for size in seq {1 .. totalSize} do
            // grey square
            storage.[size] <- storage.[size - 1]
            for tile in tiles do
                if size >= tile then
                    storage.[size] <- storage.[size] + storage.[size - tile]
        storage

    let solveImpl (totalSize: int) =
        let storage = [RedTileSize; GreenTileSize; BlueTileSize] |> generateWaysCountData totalSize
        storage.[totalSize]

    [<TestCase(5, 15L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(50, 100808458960497L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(totalSize: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, totalSize)