namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// A row of five grey square tiles is to have a number of its tiles replaced with coloured oblong tiles chosen from red (length two), green (length three), or blue (length four).
// If red tiles are chosen there are exactly seven ways this can be done:
// RREEE, ERREE, EERRE, EEERR, RRRRE, RRERR, ERRRR (where R - red, E - grey/empty)
// If green tiles are chosen there are three ways:
// GGGEE, EGGGE, EEGGG (where G - green, E - grey/empty)
// And if blue tiles are chosen there are two ways:
// BBBBE, EBBBB (where B - blue, E - grey/empty)
// Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of replacing the grey tiles in a row measuring five units in length.
// How many different ways can the grey tiles in a row measuring fifty units in length be replaced if colours cannot be mixed and at least one coloured tile must be used?

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
