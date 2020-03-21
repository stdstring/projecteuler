namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// Using a combination of grey square tiles and oblong tiles chosen from: red tiles (measuring two units), green tiles (measuring three units), and blue tiles (measuring four units),
// it is possible to tile a row measuring five units in length in exactly fifteen different ways:
// EEEEE, RREEE, ERREE, EERRE, EEERR, RRRRE, RRERR, ERRRR, GGGEE, EGGGE, EEGGG, RRGGG, GGGRR, BBBBE, EBBBB (where R - red, G - green, B - blue, E - grey/empty)
// How many ways can a row measuring fifty units in length be tiled?

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