namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem172() =

    [<Literal>]
    let DigitsCount = 10

    let getDigitUsage (radix: int) (factors: int[]) (digit: int) (index: int) =
        (index / factors.[digit]) % radix

    let setDigitUsage (radix: int) (factors: int[]) (digit: int) (usageValue: int) (index: int) =
        let oldUsageValue = (index / factors.[digit]) % radix
        index + (usageValue - oldUsageValue) * factors.[digit]

    let initStorage (maxUsageCount: int) (factors: int[]) (storage: int64[,]) =
        let radix = maxUsageCount + 1
        for digit in 1 .. 9 do
            let index = setDigitUsage radix factors digit 1 0
            storage.[0, index] <- 1L
        storage

    let fillStorage (numberSize: int) (maxUsageCount: int) (indexSize: int) (factors: int[]) (storage: int64[,]) =
        let radix = maxUsageCount + 1
        for size in 0 .. numberSize - 2 do
            for index in 0 .. indexSize - 1 do
                if storage.[size, index] <> 0L then
                    for digit in 0 .. 9 do
                        let digitUsage = index |> getDigitUsage radix factors digit
                        if digitUsage < maxUsageCount then
                            let nextIndex = index |> setDigitUsage radix factors digit (digitUsage + 1)
                            storage.[size + 1, nextIndex] <- storage.[size + 1, nextIndex] + storage.[size, index]
        storage

    let calcResult (numberSize: int) (indexSize: int) (storage: int64[,]) =
        seq {0 .. indexSize - 1} |> Seq.map (fun index -> storage.[numberSize - 1, index]) |> Seq.sum

    let solveImpl (numberSize: int) (maxUsageCount: int) =
        let radix = maxUsageCount + 1
        let indexSize = pown radix DigitsCount
        let factors = Seq.init DigitsCount (fun p -> pown radix p) |> Seq.toArray
        Array2D.zeroCreate numberSize indexSize |> initStorage maxUsageCount factors |> fillStorage numberSize maxUsageCount indexSize factors |> calcResult numberSize indexSize

    [<TestCase(18, 3, 227485267000992000L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(numberSize: int, maxUsageCount: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, numberSize, maxUsageCount)