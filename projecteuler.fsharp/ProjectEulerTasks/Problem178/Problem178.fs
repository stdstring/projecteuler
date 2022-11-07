namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem178() =

    [<Literal>]
    let DigitsCount = 10

    let digitsUsageCount = pown 2 DigitsCount

    let useDigit (digitsUsage: int) (digit: int) =
        digitsUsage ||| (1 <<< digit)

    let initData (data: int64[,]) =
        for digit in 1 .. 9 do
            data.[digit, digit |> useDigit 0] <- 1L
        data

    let rec processSize (maxSize: int) (size: int) (data: int64[,]) (count: int64) =
        match size < maxSize with
        | false -> count
        | true ->
            let nextData = Array2D.zeroCreate DigitsCount digitsUsageCount
            for usageData in 1 .. digitsUsageCount - 1 do
                // digit 0
                let usageDataFrom0 = 1 |> useDigit usageData
                nextData.[1, usageDataFrom0] <- nextData.[1, usageDataFrom0] + data.[0, usageData]
                // digits 1 - 8
                for digit in 1 .. 8 do
                    let usageDataToGreaterDigit = digit + 1 |> useDigit usageData
                    nextData.[digit + 1, usageDataToGreaterDigit] <- nextData.[digit + 1, usageDataToGreaterDigit] + data.[digit, usageData]
                    let usageDataToLessDigit = digit - 1 |> useDigit usageData
                    nextData.[digit - 1, usageDataToLessDigit] <- nextData.[digit - 1, usageDataToLessDigit] + data.[digit, usageData]
                // digit 9
                let usageDataFrom9 = 8 |> useDigit usageData
                nextData.[8, usageDataFrom9] <- nextData.[8, usageDataFrom9] + data.[9, usageData]
            seq {0 .. 9} |> Seq.map (fun digit -> nextData.[digit, digitsUsageCount - 1]) |> Seq.sum |> (+) count |> processSize maxSize (size + 1) nextData

    let solveImpl (maxSize: int) =
        processSize maxSize 1 (Array2D.zeroCreate DigitsCount digitsUsageCount |> initData) 0L

    [<TestCase(11, 4L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(12, 18L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(13, 55L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(40, 126461847755L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxSize: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxSize)