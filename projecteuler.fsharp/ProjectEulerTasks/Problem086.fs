namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System

// A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits in the opposite corner.
// By travelling on the surfaces of the room the shortest "straight line" distance from S to F is 10 and the path is shown on the diagram.
// However, there are up to three "shortest" path candidates for any given cuboid and the shortest route doesn't always have integer length.
// It can be shown that there are exactly 2060 distinct cuboids, ignoring rotations, with integer dimensions, up to a maximum size of M by M by M, for which the shortest route has integer length when M = 100.
// This is the least value of M for which the number of solutions first exceeds two thousand; the number of solutions when M = 99 is 1975.
// Find the least value of M such that the number of solutions first exceeds one million.

[<TestFixture>]
type Problem086() =

    [<Literal>]
    let MaxSize = 10000

    let createSquaresStorage (maxNumber: int) =
        let storage = Array.create (maxNumber + 1) false
        Seq.initInfinite (fun n -> n * n) |> Seq.takeWhile (fun square -> square <= maxNumber) |> Seq.iter (fun square -> storage.[square] <- true)
        storage

    // straightforward slow solution with iteration by the all sides (with O(n^3)) :
    //let calcSolutionCount (maxSize: int) =
    //    let squaresStorage = (5 * maxSize *maxSize + 1) |> createSquaresStorage
    //    let mutable count = 0
    //    for a in {1 .. maxSize} do
    //        for b in {a .. maxSize} do
    //            for c in {b .. maxSize} do
    //                let value = a * a + 2 * a * b + b * b + c * c
    //                if squaresStorage.[value] then
    //                    count <- count + 1
    //    count

    // faster solution with iteration by the sum of the sides with sizes a and b and the side with the size c (with O(n^2)) :
    let calcSolutionCount (squaresStorage: bool[]) (maxSize: int) =
        let mutable count = 0
        // check value (a + b) ^ 2 + c ^ 2
        // 1 <= a <= b <= c <= MaxSize => 2 <= a + c <= 2 * MaxSize
        for abValue in {2 .. 2 * maxSize} do
            let minAValue = max 1 (abValue - maxSize)
            let maxAValue = abValue / 2
            let minBValue = abValue - maxAValue
            let maxBValue = abValue - minAValue
            for cValue in {minBValue .. maxSize} do
                let value = abValue * abValue + cValue * cValue
                if squaresStorage.[value] then
                    count <- count + (if cValue <= maxBValue then cValue - minBValue else maxBValue - minBValue) + 1
        count

    let rec findMaxSizeRange (squaresStorage: bool[]) (minSolutionNumber: int) ((leftBorder, rightBorder): int * int) =
        let rightSolutionCount = rightBorder |> calcSolutionCount squaresStorage
        match rightSolutionCount >= minSolutionNumber with
        | true -> leftBorder, rightBorder
        | false -> (rightBorder, 2 * rightBorder) |> findMaxSizeRange squaresStorage minSolutionNumber

    let rec findMaxSize (squaresStorage: bool[]) (minSolutionNumber: int) ((leftBorder, rightBorder): int * int) =
        match rightBorder - leftBorder with
        | 1 -> rightBorder
        | _ ->
            let middlePoint = (rightBorder + leftBorder) / 2
            match middlePoint |> calcSolutionCount squaresStorage with
            | middleValue when middleValue = minSolutionNumber -> middlePoint
            | middleValue when middleValue < minSolutionNumber -> (middlePoint, rightBorder) |> findMaxSize squaresStorage minSolutionNumber
            | middleValue when middleValue > minSolutionNumber -> (leftBorder, middlePoint) |> findMaxSize squaresStorage minSolutionNumber
            | _ -> failwith "Unexpected branch of match expression"

    let solveImpl (minSolutionNumber: int) =
        // Description (see picture in https://projecteuler.net/problem=86):
        // Let a <= b <= c <= M (for ignoring rotations)
        // Path consists of two straight segment. Let the first segment lies in the side with a * c sizes and the second segment lies in the side with b * c sizes.
        // Let crosspoint between the path and side with size c divides side with size c on the segments with sizes t and (c - t) correspondingly
        // L = L1 + L2 where L - length of the full path, L1 - length of the first segment, L2 - length of the second segment
        // L1 = (a^2 + t^2)^(1/2), L2 = (b^2 + (c - t)^2)^(1/2)
        // From condition L -> min, we have the following equation: dL / dt = 0
        // dL / dt = t * (a^2 + t^2)^(-1/2) - (c - t) * (b^2 + (c - t)^2)^(-1/2)
        // From the eqation dL / dt = 0, we have the following t = a * c / (a + b)
        // Thus, L1 = a * ((a + b)^2 + c^2)^(1/2) / (a + b), L2 = b * ((a + b)^2 + c^2)^(1/2) / (a + b), L = L1 + L2 = ((a + b)^2 + c^2)^(1/2)
        // From the condition L - natural number, we have the following: value (a + b)^2 + c^2 must be full square for some natural number
        // PS. It is easy to show, that such choose of the sides of the our rectangle gives the minimum value for the path
        let squaresStorage = (5 * MaxSize * MaxSize + 1) |> createSquaresStorage
        let leftBorder = 10
        if (leftBorder |> calcSolutionCount squaresStorage) >= minSolutionNumber then
            InvalidOperationException() |> raise
        (leftBorder, 2 * leftBorder) |> findMaxSizeRange squaresStorage minSolutionNumber |> findMaxSize squaresStorage minSolutionNumber

    [<TestCase(2000, 100, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 1818, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(minSolutionNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, minSolutionNumber)
