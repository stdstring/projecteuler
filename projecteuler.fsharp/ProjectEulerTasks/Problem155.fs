namespace ProjectEulerTasks

open CommonLib.Rational
open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

// An electric circuit uses exclusively identical capacitors of the same value C.
// The capacitors can be connected in series or in parallel to form sub-units, which can then be connected in series or in parallel with other capacitors or other sub-units to form larger sub-units, and so on up to a final circuit.
// Using this simple procedure and up to n identical capacitors, we can make circuits having a range of different total capacitances.
// For example, using up to n=3 capacitors of 60 mcF each, we can obtain the following 7 distinct total capacitance values (corresponding picture you can see here https://projecteuler.net/problem=155)
// If we denote by D(n) the number of distinct total capacitance values we can obtain when using up to n equal-valued capacitors and the simple procedure described above, we have: D(1)=1, D(2)=3, D(3)=7 ...
// Find D(18).
// Reminder : When connecting capacitors C1, C2 etc in parallel, the total capacitance is C = C1 + C2 + ..., whereas when connecting them in series, the overall capacitance is given by: (1/C) = (1/C1) + (1/C2) + ...

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
