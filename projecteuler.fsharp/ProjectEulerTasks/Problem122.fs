namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// The most naive way of computing n^15 requires fourteen multiplications:
// n * n * ... * n = n^15
// But using a "binary" method you can compute it in six multiplications:
// n * n = n^2
// n^2 * n^2 = n^4
// n^4 * n^4 = n^8
// n^8 * n^4 = n^12
// n^12 * n^2 = n^14
// n^14 * n = n^15
// However it is yet possible to compute it in only five multiplications:
// n * n = n^2
// n^2 * n = n^3
// n^3 * n^3 = n^6
// n^6 * n^6 = n^12
// n^12 * n^3 = n^15
// We shall define m(k) to be the minimum number of multiplications to compute n^k; for example m(15) = 5.
// For 1 <= k <= 200, find Sum(m(k)).


// TODO (std_string) : think about moving into CommonLibs
type BitVector(size: int) =

    let storage: uint32[] = 1 + (size / 32) |> Array.zeroCreate

    member public this.Get(index: int) =
        let cell = index / 32
        let bit = index % 32
        (storage.[cell] &&& (1u <<< bit)) > 0u

    member public this.Set(index: int, value: bool) =
        let cell = index / 32
        let bit = index % 32
        if value then
            storage.[cell] <- storage.[cell] ||| (1u <<< bit)
        else
            storage.[cell] <- storage.[cell] &&& ~~~(1u <<< bit)

    member public this.CopyFrom(vector: BitVector) =
        vector.Storage |> Array.iteri (fun index value -> storage.[index] <- value)

    member private this.Storage: uint32[] = storage

module Problem122Impl =
    type BestData = {Count: int; Patterns: ResizeArray<BitVector>}

open Problem122Impl

[<TestFixture>]
type Problem122() =

    let createPattern (patternSize: int) (number: int) (factorPattern: BitVector) =
        let pattern = patternSize |> BitVector
        factorPattern |> pattern.CopyFrom
        pattern.Set(number, true)
        pattern

    let createBestData (count: int) (patterns: seq<BitVector>) =
        {BestData.Count = count; BestData.Patterns = patterns |> ResizeArray<BitVector>}

    let filterPatterns (power: int) (patterns: seq<BitVector>) =
        patterns |> Seq.filter (fun pattern -> pattern.Get(power)) |> Seq.toList

    let rec findBestFactors (patternSize: int) (storage: BestData[]) (power: int) (bestFactors: BestData) (factorPower: int) =
        let otherFactorPower = power - factorPower
        match factorPower < otherFactorPower with
        | true -> bestFactors
        | false ->
            match storage.[factorPower].Patterns |> filterPatterns otherFactorPower with
            | [] -> (factorPower - 1) |> findBestFactors patternSize storage power bestFactors
            | filteredPatterns ->
                match storage.[factorPower].Count with
                | value when value + 1 < bestFactors.Count ->
                    let newBestData = filteredPatterns |> Seq.map (fun pattern -> pattern |> createPattern patternSize power) |> createBestData (value + 1)
                    (factorPower - 1) |> findBestFactors patternSize storage power newBestData
                | value when value + 1 = bestFactors.Count ->
                    filteredPatterns |> Seq.iter (fun pattern -> pattern |> createPattern patternSize power |> bestFactors.Patterns.Add)
                    (factorPower - 1) |> findBestFactors patternSize storage power bestFactors
                | _ -> (factorPower - 1) |> findBestFactors patternSize storage power bestFactors

    let solveImpl (maxPower: int) =
        let patternSize = maxPower + 1
        let storage = maxPower + 1 |> Array.zeroCreate
        storage.[0] <- {BestData.Count = 0; BestData.Patterns = ResizeArray<BitVector>()}
        let patternFor1 = patternSize |> BitVector
        patternFor1.Set(1, true)
        storage.[1] <- {BestData.Count = 0; BestData.Patterns = ResizeArray<BitVector>([patternFor1])}
        for power in seq {2 .. maxPower} do
            let initBestFactors = storage.[power - 1].Patterns |> Seq.map (fun pattern -> pattern |> createPattern patternSize power) |> createBestData (storage.[power - 1].Count + 1)
            let bestFactors = findBestFactors patternSize storage power initBestFactors (power - 2)
            storage.[power] <- bestFactors
        storage |> Seq.sumBy (fun data -> data.Count)

    [<TestCase(200, 1582, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxPower: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxPower)
