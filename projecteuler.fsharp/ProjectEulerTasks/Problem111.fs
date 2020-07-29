namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// Considering 4-digit primes containing repeated digits it is clear that they cannot all be the same: 1111 is divisible by 11, 2222 is divisible by 22, and so on.
// But there are nine 4-digit primes containing three ones: 1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111
// We shall say that M(n, d) represents the maximum number of repeated digits for an n-digit prime where d is the repeated digit,
// N(n, d) represents the number of such primes, and S(n, d) represents the sum of these primes.
// So M(4, 1) = 3 is the maximum number of repeated digits for a 4-digit prime where one is the repeated digit,
// there are N(4, 1) = 9 such primes, and the sum of these primes is S(4, 1) = 22275.
// It turns out that for d = 0, it is only possible to have M(4, 0) = 2 repeated digits, but there are N(4, 0) = 13 such cases.
// In the same way we obtain the following results for 4-digit primes:
// M(4, 0) = 2, N(4, 0) = 13, S(4, 0) = 67061
// M(4, 1) = 3, N(4, 1) = 9,  S(4, 1) = 22275
// M(4, 2) = 3, N(4, 2) = 1,  S(4, 2) = 2221
// M(4, 3) = 3, N(4, 3) = 12, S(4, 3) = 46214
// M(4, 4) = 3, N(4, 4) = 2,  S(4, 4) = 8888
// M(4, 5) = 3, N(4, 5) = 1,  S(4, 5) = 5557
// M(4, 6) = 3, N(4, 6) = 1,  S(4, 6) = 6661
// M(4, 7) = 3, N(4, 7) = 9,  S(4, 7) = 57863
// M(4, 8) = 3, N(4, 8) = 1,  S(4, 8) = 8887
// M(4, 9) = 3, N(4, 9) = 7,  S(4, 9) = 48073
// For d = 0 to 9, the sum of all S(4, d) is 273700.
// Find the sum of all S(10, d).

[<TestFixture>]
type Problem111() =

    let lastDigitValue = [1; 3; 7; 9]

    // TODO (std_string) : think about moving this method into CommonLibs.NumbersDividers class
    let isPrime (primes: int64 list) (number: int64) =
        let rec isPrimeImpl (primes: int64 list) =
            match primes with
            | [] -> true
            | prime :: _ when prime * prime > number -> true
            | prime :: _ when number % prime = 0L -> false
            | _ :: primesRest -> primesRest |> isPrimeImpl
        primes |> isPrimeImpl

    let generateMainDigitMasks (digitsCount: int) (mainDigit: int) (mainDigitsCount: int) =
        let rec generateImpl (index: int) (fromPos: int) (currentMask: int) (generatedMasks: ResizeArray<int>) =
            match index < mainDigitsCount with
            | true ->
                // 0 can't be the first digit
                let maxDigitPos = digitsCount - mainDigitsCount + index - (if mainDigit = 0 then 1 else 0)
                for digitPos in fromPos .. maxDigitPos do
                    generatedMasks |> generateImpl (index + 1) (digitPos + 1) (currentMask ||| (1 <<< digitPos))
            | false -> currentMask |> generatedMasks.Add
        let storage = new ResizeArray<int>()
        // 0, 2, 4, 5, 6, 8 can't be the last digit
        storage |> generateImpl 0 (if lastDigitValue |> List.contains mainDigit then 0 else 1) 0
        storage

    let generateMainDigitPart (digit: int) (digitsCount: int) (digitsFactors: int64[]) (usedDigitsMask: int) =
        seq {0 .. digitsCount - 1} |> Seq.filter (fun digitPos -> (usedDigitsMask &&& (1 <<< digitPos)) > 0) |> Seq.map (fun digitPos -> digitsFactors.[digitPos]) |> Seq.sum |> (*) (digit |> int64)

    let createDigitsRange (digitsCount: int) (digitPos: int) =
        match digitPos with
        | 0 -> lastDigitValue
        | _ when digitPos = digitsCount - 1 -> [1 .. 9]
        | _ -> [0 .. 9]

    let generateAllPossibleNumbers (mainDigit: int) (digitsCount: int) (digitsFactors: int64[]) (mainDigitPart: int64) (usedDigitsMask: int) =
        let rec generateImpl (digitPos: int) (generatedPart: int64) (generatedDigits: ResizeArray<int64>) =
            match digitPos = digitsCount with
            | true -> mainDigitPart + generatedPart |> generatedDigits.Add
            | false ->
                match usedDigitsMask &&& (1 <<< digitPos) with
                | 0 ->
                    for digit in digitPos |> createDigitsRange digitsCount do
                        if digit <> mainDigit then
                            generatedDigits |> generateImpl (digitPos + 1) (generatedPart + (digit |> int64) * digitsFactors.[digitPos])
                | _ -> generatedDigits |> generateImpl (digitPos + 1) generatedPart
        let storage = new ResizeArray<int64>()
        storage |> generateImpl 0 0L
        storage

    let processMainDigitMask (primes: int64 list) (digitsCount: int) (digitsFactors: int64[]) (mainDigit: int) (mainDigitMask: int) =
        let mainDigitPart = if mainDigit = 0 then 0L else mainDigitMask |> generateMainDigitPart mainDigit digitsCount digitsFactors
        mainDigitMask |> generateAllPossibleNumbers mainDigit digitsCount digitsFactors mainDigitPart |> Seq.filter (fun number -> number |> isPrime primes) |> Seq.map int64 |> Seq.sum

    let processDigit (primes: int64 list) (digitsCount: int) (digitsFactors: int64[]) (digit: int) =
        let rec processImpl (mainDigitsCount: int) =
            let primesSum = mainDigitsCount |> generateMainDigitMasks digitsCount digit |> Seq.map (fun mainDigitMask -> mainDigitMask |> processMainDigitMask primes digitsCount digitsFactors digit) |> Seq.sum
            match primesSum with
            | 0L -> (mainDigitsCount - 1) |> processImpl
            | value -> value
        // if digit is 0, then first and last digits (at least) must not equal 0
        (digitsCount - if digit = 0 then 2 else 1) |> processImpl

    let solveImpl (digitsCount: int) =
        let maxPrime = pown 10.0 digitsCount |> sqrt |> ceil |> int
        let primes = EratosSieve.Create(maxPrime).ToSeq() |> Seq.map int64 |> Seq.toList
        let digitsFactors = Seq.init digitsCount (fun p -> pown 10 p |> int64) |> Seq.toArray
        seq {0 .. 9} |> Seq.map (fun digit -> digit |> processDigit primes digitsCount digitsFactors) |> Seq.sum

    [<TestCase(4, 273700L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(5, 6045857L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(9, 45609504098L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10, 612407567715L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(digitsCount: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, digitsCount)
