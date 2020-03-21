namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// A row measuring seven units in length has red blocks with a minimum length of three units placed on it, such that any two red blocks (which are allowed to be different lengths) are separated by at least one grey square.
// There are exactly seventeen ways of doing this:
// EEEEEEE, RRREEEE, ERRREEE, EERRREE, EEERRRE, EEEERRR, RRRERRR, RRRREEE, ERRRREE, EERRRRE, EEERRRR, RRRRREE, ERRRRRE, EERRRRR, RRRRRRE, ERRRRRR, RRRRRRR (where R - red, E - grey/empty)
// How many ways can a row measuring fifty units in length be filled?
// NOTE: Although the example above does not lend itself to the possibility, in general it is permitted to mix block sizes. For example, on a row measuring eight units in length you could use red (3), grey (1), and red (4).

module Problem114Impl =
    type RowWaysCountData = {EndedByGrey: int64; EndedByRed: int64}

open Problem114Impl

[<TestFixture>]
type Problem114() =

    [<Literal>]
    let MinRedBlockSize = 3

    let generateWaysCountData (totalSize: int) =
        let storage = (totalSize + 1) |> Array.zeroCreate
        storage.[0] <- {RowWaysCountData.EndedByGrey = 1L; RowWaysCountData.EndedByRed = 0L}
        for size in {1 .. totalSize} do
            let endedByGrey = storage.[size - 1].EndedByGrey + storage.[size - 1].EndedByRed
            let endedByRed = seq {MinRedBlockSize .. size} |> Seq.sumBy (fun redBlockSize -> storage.[size - redBlockSize].EndedByGrey)
            storage.[size] <- {RowWaysCountData.EndedByGrey = endedByGrey; RowWaysCountData.EndedByRed = endedByRed}
        storage

    let solveImpl (totalSize: int) =
        let storage = totalSize |> generateWaysCountData
        storage.[totalSize].EndedByGrey + storage.[totalSize].EndedByRed

    [<TestCase(7, 17L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(50, 16475640049L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(totalSize: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, totalSize)