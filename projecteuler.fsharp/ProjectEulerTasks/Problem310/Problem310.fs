namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

[<TestFixture>]
type Problem310() =

    // Sprague-Grundy function (see https://www.gabrielnivasch.org/fun/combinatorial-games/sprague-grundy)
    let rec calcSGFunctionValue (valuesTo: HashSet<int>) (number: int) =
        match number |> valuesTo.Contains with
        | true -> number + 1 |> calcSGFunctionValue valuesTo
        | false -> number

    let generateCounts (bStones: ResizeArray<int>) (cStones: ResizeArray<int>) =
        let counts = bStones.Count |> Array.zeroCreate
        let mutable cIndex = cStones.Count - 1
        for bIndex in seq {bStones.Count - 1 .. -1 .. 0} do
            while (cIndex >= 0) && (bStones.[bIndex] <= cStones.[cIndex]) do
                cIndex <- cIndex - 1
            if bIndex < bStones.Count - 1 then
                counts.[bIndex] <- counts.[bIndex + 1]
            counts.[bIndex] <- counts.[bIndex] + (cStones.Count - cIndex - 1)
        counts

    let calcLosingPositionsCount (aStones: ResizeArray<int>) (bStones: ResizeArray<int>) (cStones: ResizeArray<int>) =
        let counts = generateCounts bStones cStones
        let mutable losingPositionsCount = 0L
        let mutable bIndex = 0
        for aIndex in seq {0 .. aStones.Count - 1} do
            while (bIndex < counts.Length) && (bStones.[bIndex] < aStones.[aIndex]) do
                bIndex <- bIndex + 1
            if bIndex < bStones.Count then
                losingPositionsCount <- losingPositionsCount + (counts.[bIndex] |> int64)
        losingPositionsCount

    let solveImpl (stonesCount: int) =
        // calc Sprague-Grundy function
        let squares = (+) 1 |> Seq.initInfinite |> Seq.map (fun n -> n * n) |> Seq.takeWhile (fun n -> n <= stonesCount)
        let sgFunction = stonesCount + 1 |> Array.zeroCreate
        for stone in seq {1 .. stonesCount} do
            let valuesTo = squares |> Seq.takeWhile (fun n -> n <= stone) |> Seq.fold (fun (values: HashSet<int>) n -> sgFunction.[stone - n] |> values.Add |> ignore; values) (new HashSet<int>())
            sgFunction.[stone] <- calcSGFunctionValue valuesTo 0
        // group states with same Sprague-Grundy function value
        let sgFunctionGroups = new ResizeArray<ResizeArray<int>>()
        for stone in seq {0 .. stonesCount} do
            let sgFunctionValue = sgFunction.[stone]
            if sgFunctionGroups.Count = sgFunctionValue then
                new ResizeArray<int>() |> sgFunctionGroups.Add
            stone |> sgFunctionGroups.[sgFunctionValue].Add
        // calc loosing positions
        let maxGroup = sgFunctionGroups.Count - 1
        seq {for a in 0 .. maxGroup do
             for b in 0 .. maxGroup do
                 let c = a ^^^ b
                 if c <= maxGroup then
                     yield calcLosingPositionsCount sgFunctionGroups.[a] sgFunctionGroups.[b] sgFunctionGroups.[c]} |> Seq.sum

    [<TestCase(29, 1160L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100000, 2586528661783L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(stonesCount: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, stonesCount)
