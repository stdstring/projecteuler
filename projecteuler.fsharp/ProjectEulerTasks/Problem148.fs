namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// We can easily verify that none of the entries in the first seven rows of Pascal's triangle are divisible by 7:
//                        1
//                    1       1
//                1       2       1
//            1       3       3       1
//        1       4       6       4       1
//    1       5      10      10       5       1
// 1      6      15      20      15       6       1
// However, if we check the first one hundred rows, we will find that only 2361 of the 5050 entries are not divisible by 7.
// Find the number of entries which are not divisible by 7 in the first one billion (10^9) rows of Pascal's triangle.

module Problem148Impl =
    type BinominalCoeffData = {Factor: int64; NonDivisibleCount: int64}

open Problem148Impl

[<TestFixture>]
type Problem148() =

    // direct calculation (slow)
    (*let solveImpl (rowCount: int) (primeFactor: int) =
        let mutable result = 0L
        for number in 0 .. rowCount - 1 do
            let digits = CommonLib.NumbersDigits.GetDigits(number, primeFactor)
            let value = digits |> List.fold (fun result digit -> result * (digit + 1)) 1 |> int64
            result <- result + value
        result*)

    let calcNonDivisibleCountForRange (toDigit: int64) =
        seq {0L .. toDigit} |> Seq.map (fun digit -> digit + 1L) |> Seq.sum

    let initData (rowCount: int64) (primeFactor: int64) =
        let nonDivisibleCount = primeFactor - 1L |> calcNonDivisibleCountForRange
        let storage = new ResizeArray<BinominalCoeffData>([{BinominalCoeffData.Factor = primeFactor; BinominalCoeffData.NonDivisibleCount = nonDivisibleCount}])
        // fill
        while (storage.[storage.Count - 1].Factor * primeFactor) < rowCount do
            let lastData = storage.[storage.Count - 1]
            {BinominalCoeffData.Factor = lastData.Factor * primeFactor; BinominalCoeffData.NonDivisibleCount = lastData.NonDivisibleCount * nonDivisibleCount} |> storage.Add
        storage

    let calcNonDivisibleCount (maxRow: int64) (data: ResizeArray<BinominalCoeffData>) =
        let rec calcImpl (index: int) (product: int64) (number: int64) (result: int64) =
            match number < data.[0].Factor with
            | true -> result + product * (number |> calcNonDivisibleCountForRange)
            | false ->
                match number / data.[index].Factor with
                | 0L -> result |> calcImpl (index - 1) product number: int64
                | digit ->
                    let updatedResult = result + product * (digit - 1L |> calcNonDivisibleCountForRange) * data.[index].NonDivisibleCount
                    updatedResult |> calcImpl (index - 1) (product * (1L + digit)) (number - digit * data.[index].Factor)
        calcImpl (data.Count - 1) 1L maxRow 0L

    let solveImpl (rowCount: int) (primeFactor: int) =
        // Description: we the following consequence from the Lucas's theorem (https://en.wikipedia.org/wiki/Lucas%27s_theorem):
        // In the sequence of binomial coefficients (n, 0), (n, 1), ..., (n, n), the count of not multiples of prime p is (a1 + 1) * ... * (am + 1),
        // where a1, ..., am - digits of p-ary notation of number n and m = floor(ln(n)/ln(p)) + 1 - its length
        let rowCount = rowCount |> int64
        primeFactor |> int64 |> initData rowCount |> calcNonDivisibleCount (rowCount - 1L)

    [<TestCase(7, 7, 28L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 7, 2361L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(60, 7, 880L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000000, 7, 2129970655314432L, TimeThresholds.HardTimeLimit)>] //?
    member public this.Solve(rowCount: int, primeFactor: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, rowCount, primeFactor)
