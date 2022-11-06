namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

module Problem191Impl =
    type RowWaysCountData = {EndedByO: int; EndedBySingleA: int; EndedByDoubleA: int; Total: int}

open Problem191Impl

[<TestFixture>]
type Problem191() =

    let generateWaysCountData (totalDaysPeriod: int) =
        let storage = (totalDaysPeriod + 1) |> Array.zeroCreate
        storage.[0] <- {RowWaysCountData.EndedByO = 1; RowWaysCountData.EndedBySingleA = 0; RowWaysCountData.EndedByDoubleA = 0; RowWaysCountData.Total = 1}
        for daysPeriod in {1 .. totalDaysPeriod} do
            let endedByO = storage.[daysPeriod - 1].EndedByO + storage.[daysPeriod - 1].EndedBySingleA + storage.[daysPeriod - 1].EndedByDoubleA
            let endedBySingleA = storage.[daysPeriod - 1].EndedByO
            let endedByDoubleA = storage.[daysPeriod - 1].EndedBySingleA
            let total = endedByO + endedBySingleA + endedByDoubleA
            storage.[daysPeriod] <- {RowWaysCountData.EndedByO = endedByO; RowWaysCountData.EndedBySingleA = endedBySingleA; RowWaysCountData.EndedByDoubleA = endedByDoubleA; RowWaysCountData.Total = total}
        storage

    let solveImpl (totalDaysPeriod: int) =
        let storage = totalDaysPeriod |> generateWaysCountData
        storage.[totalDaysPeriod].Total + (seq {1 .. totalDaysPeriod} |> Seq.map (fun daysPeriod -> storage.[daysPeriod - 1].Total * storage.[totalDaysPeriod - daysPeriod].Total) |> Seq.sum)

    [<TestCase(4, 43, TimeThresholds.HardTimeLimit)>]
    [<TestCase(30, 1918080160, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(daysPeriod: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, daysPeriod)