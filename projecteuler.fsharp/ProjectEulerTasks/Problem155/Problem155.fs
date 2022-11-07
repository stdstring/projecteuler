namespace ProjectEulerTasks

open CommonLib.Rational
open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

[<TestFixture>]
type Problem155() =

    [<Literal>]
    let CapacitorValue = 60

    // TODO (std_string) : think about another approach: this is very slow
    let solveImpl (maxCount: int) =
        let storage = Array.zeroCreate (maxCount + 1)
        storage.[1] <- new ResizeArray<RationalNumber64>([new RationalNumber64(CapacitorValue)])
        let distinctCapacitorsValues = new HashSet<RationalNumber64>([new RationalNumber64(CapacitorValue)]) :> ISet<RationalNumber64>
        for count in 2 .. maxCount do
            let distinctCapacitorsValuesForCount = new HashSet<RationalNumber64>() :> ISet<RationalNumber64>
            storage.[count] <- new ResizeArray<RationalNumber64>()
            for smallCount in 1 .. count / 2 do
                let bigCount = count - smallCount
                for smallValue in storage.[smallCount] do
                    for bigValue in storage.[bigCount] do
                        let parallelValue = (smallValue + bigValue).Simplify()
                        let serialValue = (smallValue * bigValue / (smallValue + bigValue)).Simplify()
                        parallelValue |> distinctCapacitorsValues.Add |> ignore
                        serialValue |> distinctCapacitorsValues.Add |> ignore
                        if parallelValue |> distinctCapacitorsValuesForCount.Add then
                            parallelValue |> storage.[count].Add
                        if serialValue |> distinctCapacitorsValuesForCount.Add then
                            serialValue |> storage.[count].Add
        distinctCapacitorsValues.Count

    [<TestCase(2, 3, TimeThresholds.HardTimeLimit)>]
    [<TestCase(3, 7, TimeThresholds.HardTimeLimit)>]
    [<TestCase(18, 3857447, TimeThresholds.MaxTimeLimit)>]
    member public this.Solve(maxCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxCount)
