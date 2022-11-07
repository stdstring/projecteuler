namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

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
