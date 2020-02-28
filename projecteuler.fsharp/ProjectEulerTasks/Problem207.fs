namespace ProjectEulerTasks

open CommonLib.Rational
open NUnit.Framework
open ProjectEulerTasks.Utils

// For some positive integers k, there exists an integer partition of the form 4^t = 2^t + k, where 4^t, 2^t, and k are all positive integers and t is a real number.
// The first two such partitions are 4^1 = 2^1 + 2 and 4^(1.5849625...) = 2^(1.5849625...) + 6.
// Partitions where t is also an integer are called perfect.
// For any m => 1 let P(m) be the proportion of such partitions that are perfect with k <= m.
// Thus P(6) = 1/2.
// In the following table are listed some values of P(m):
// P(5) = 1/1
// P(10) = 1/2
// P(15) = 2/3
// P(20) = 1/2
// P(25) = 1/2
// P(30) = 2/5
// ...
// P(180) = 1/4
// P(185) = 3/13
// Find the smallest m for which P(m) < 1/12345

module Problem207Impl =
    type RangeData = {Left: int; Right: int; PerfectCount: int}

open Problem207Impl

[<TestFixture>]
type Problem207() =

    [<Literal>]
    let StartNumber = 2

    let rec searchRange (border: RationalNumber32) (rangeData: RangeData) =
        let currentBorder = RationalNumber32(rangeData.PerfectCount, rangeData.Right - 1 - StartNumber + 1)
        match currentBorder < border with
        | true -> rangeData
        | false -> {RangeData.Left = rangeData.Right; RangeData.Right = rangeData.Right * 2; RangeData.PerfectCount = rangeData.PerfectCount + 1} |> searchRange border

    let rec binarySearch (left: int) (right: int) (perfectCount: int) (border: RationalNumber32) =
        let leftValue = RationalNumber32(perfectCount, left - StartNumber + 1)
        let rightValue = RationalNumber32(perfectCount, right - StartNumber + 1)
        match left with
        | _ when left = right -> left
        | _ when (left + 1 = right) && leftValue < border -> left
        | _ when (left + 1 = right) && rightValue < border -> right
        | _ when leftValue = border -> left + 1
        | _ ->
            let middle = (left + right) / 2
            let middleValue = RationalNumber32(perfectCount, middle - StartNumber + 1)
            match middleValue with
            | _ when middleValue = border -> middle + 1
            | _ when middleValue < border -> binarySearch left middle perfectCount border
            | _ -> binarySearch middle right perfectCount border

    let searchXValue (border: RationalNumber32) (rangeData: RangeData) =
        let perfectCount = rangeData.PerfectCount
        let left = rangeData.Left
        let right = rangeData.Right
        match RationalNumber32(perfectCount, left + 1 - StartNumber + 1) with
        | value when value < border -> left
        | value when value = border -> left + 1
        | _ -> binarySearch left right perfectCount border

    let solveImpl (borderNumerator: int) (borderDenominator: int) =
        let border = RationalNumber32(borderNumerator, borderDenominator)
        let rangeData = {RangeData.Left = 4; RangeData.Right = 8; RangeData.PerfectCount = 2} |> searchRange border
        // Let 2^t = x, then our expression has the following form x^2 = x + k
        let xValue = rangeData |> searchXValue border |> int64
        xValue * xValue - xValue

    [<TestCase(1, 12345, 44043947822L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(borderNumerator: int, borderDenominator: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, borderNumerator, borderDenominator)
