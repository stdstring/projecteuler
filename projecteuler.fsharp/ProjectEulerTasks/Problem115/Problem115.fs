namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem115Impl =
    type RowWaysCountData = {EndedByGrey: int; EndedByRed: int}

open Problem115Impl

[<TestFixture>]
type Problem115() =

    let calcNextWaysCount (minRedBlockSize: int) (storage: ResizeArray<RowWaysCountData>) =
        let size = storage.Count
        let endedByGrey = storage.[size - 1].EndedByGrey + storage.[size - 1].EndedByRed
        let endedByRed = seq {minRedBlockSize .. size} |> Seq.sumBy (fun redBlockSize -> storage.[size - redBlockSize].EndedByGrey)
        {RowWaysCountData.EndedByGrey = endedByGrey; RowWaysCountData.EndedByRed = endedByRed}

    let initWaysCountStorage (minRedBlockSize: int) =
        let storage = new ResizeArray<RowWaysCountData>()
        {RowWaysCountData.EndedByGrey = 1; RowWaysCountData.EndedByRed = 0} |> storage.Add
        for _ in {1 .. minRedBlockSize} do
            storage |> calcNextWaysCount minRedBlockSize |> storage.Add
        storage

    let rec findMinRowSize (minRedBlockSize: int) (minWaysCount: int) (storage: ResizeArray<RowWaysCountData>) =
        let nextWaysCount = storage |> calcNextWaysCount minRedBlockSize
        let waysCount = nextWaysCount.EndedByGrey + nextWaysCount.EndedByRed |> int
        match waysCount > minWaysCount with
        | true -> storage.Count
        | false ->
            nextWaysCount |> storage.Add
            storage |> findMinRowSize minRedBlockSize minWaysCount

    let solveImpl (minRedBlockSize: int) (minWaysCount: int) =
        minRedBlockSize |> initWaysCountStorage |> findMinRowSize minRedBlockSize minWaysCount

    [<TestCase(3, 1000000, 30, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10, 1000000, 57, TimeThresholds.HardTimeLimit)>]
    [<TestCase(50, 1000000, 168, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(minRedBlockSize: int, minWaysCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, minRedBlockSize, minWaysCount)
