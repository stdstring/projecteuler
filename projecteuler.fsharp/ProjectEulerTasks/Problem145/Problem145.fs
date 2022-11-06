namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem145() =

    let checkSum (number: int) =
        number |> NumbersDigits.GetDigits |> List.exists (fun digit -> digit % 2 = 0) |> not

    let calcForLess100 () =
        seq {10 .. 99} |>
        Seq.map (fun number -> number, number |> NumbersDigits.ReverseNumber) |>
        Seq.filter (fun (number, reversedNumber) -> reversedNumber >= number) |>
        Seq.map (fun (number, reversedNumber) -> number + reversedNumber) |>
        Seq.filter (fun sum -> sum |> checkSum) |>
        Seq.length |>
        (*) 2

    let calcForEvenDigitsHalfNumberRange (rangeStart: int) (rangeFinish: int) (digitsCount: int) =
        let rec processDigits (digits: int list) (count: int) =
            match digits with
            // last digit
            | [0] -> 0
            | [1] -> 0
            | [2] -> count // 1
            | [3] -> count // 2
            | [4] -> count * 2 // 1, 3
            | [5] -> count * 2 // 2, 4
            | [6] -> count * 2 // 1, 3
            | [7] -> count // 2
            | [8] -> count // 1
            | [9] -> 0
            // non-last digit
            | 0 :: digitsRest -> (count * 5) |> processDigits digitsRest // 1, 3, 5, 7, 9
            | 1 :: digitsRest -> (count * 5) |> processDigits digitsRest // 0, 2, 4, 6, 8
            | 2 :: digitsRest -> (count * 4) |> processDigits digitsRest // 1, 3, 5, 7
            | 3 :: digitsRest -> (count * 4) |> processDigits digitsRest // 0, 2, 4, 6
            | 4 :: digitsRest -> (count * 3) |> processDigits digitsRest // 1, 3, 5
            | 5 :: digitsRest -> (count * 3) |> processDigits digitsRest // 0, 2, 4
            | 6 :: digitsRest -> (count * 2) |> processDigits digitsRest // 1, 3
            | 7 :: digitsRest -> (count * 2) |> processDigits digitsRest // 0, 2
            | 8 :: digitsRest -> count |> processDigits digitsRest // 1
            | 9 :: digitsRest -> count |> processDigits digitsRest // 0
            | _ -> failwith "Unexpected branch of match expression"
        seq {rangeStart .. rangeFinish} |> Seq.map (fun number -> NumbersDigits.GetFixedSizeDigits(number, digitsCount)) |> Seq.map (fun digits -> 2 * processDigits digits 1) |> Seq.sum

    let calcForOddDigitsHalfNumberRange (rangeStart: int) (rangeFinish: int) (digitsCount: int) =
        // digitIndex = 0, 1, 2, ...
        let rec processDigits (digits: int list) (digitIndex: int) (count: int) =
            match digits with
            // last digit
            | [0] -> 0
            | [1] -> 0
            | [2] -> 0
            | [3] -> 0
            | [4] -> 0
            | [5] -> 0
            | [6] -> count // 5
            | [7] -> 2 * count // 4, 6
            | [8] -> 3 * count // 3, 5, 7
            | [9] -> 4 * count // 2, 4, 6, 8
            // non-last digit & even digitIndex
            | 0 :: _ when digitIndex % 2 = 0 -> 0
            | 1 :: _ when digitIndex % 2 = 0 -> 0
            | 2 :: digitsRest when digitIndex % 2 = 0 -> count |> processDigits digitsRest (digitIndex + 1) // 9
            | 3 :: digitsRest when digitIndex % 2 = 0 -> count |> processDigits digitsRest (digitIndex + 1) // 8
            | 4 :: digitsRest when digitIndex % 2 = 0 -> (count * 2) |> processDigits digitsRest (digitIndex + 1) // 7, 9
            | 5 :: digitsRest when digitIndex % 2 = 0 -> (count * 2) |> processDigits digitsRest (digitIndex + 1) // 6, 8
            | 6 :: digitsRest when digitIndex % 2 = 0 -> (count * 3) |> processDigits digitsRest (digitIndex + 1) // 5, 7, 9
            | 7 :: digitsRest when digitIndex % 2 = 0 -> (count * 3) |> processDigits digitsRest (digitIndex + 1) // 4, 6, 8
            | 8 :: digitsRest when digitIndex % 2 = 0 -> (count * 4) |> processDigits digitsRest (digitIndex + 1) // 3, 5, 7, 9
            | 9 :: digitsRest when digitIndex % 2 = 0 -> (count * 4) |> processDigits digitsRest (digitIndex + 1) // 2, 4, 6, 8
            // non-last digit & odd digitIndex
            | 0 :: digitsRest when digitIndex % 2 = 1 -> (count * 5) |> processDigits digitsRest (digitIndex + 1) // 0, 2, 4, 6, 8
            | 1 :: digitsRest when digitIndex % 2 = 1 -> (count * 4) |> processDigits digitsRest (digitIndex + 1) // 1, 3, 5, 7
            | 2 :: digitsRest when digitIndex % 2 = 1 -> (count * 4) |> processDigits digitsRest (digitIndex + 1) // 0, 2, 4, 6
            | 3 :: digitsRest when digitIndex % 2 = 1 -> (count * 3) |> processDigits digitsRest (digitIndex + 1) // 1, 3, 5
            | 4 :: digitsRest when digitIndex % 2 = 1 -> (count * 3) |> processDigits digitsRest (digitIndex + 1) // 0, 2, 4
            | 5 :: digitsRest when digitIndex % 2 = 1 -> (count * 2) |> processDigits digitsRest (digitIndex + 1) // 1, 3
            | 6 :: digitsRest when digitIndex % 2 = 1 -> (count * 2) |> processDigits digitsRest (digitIndex + 1) // 0, 2
            | 7 :: digitsRest when digitIndex % 2 = 1 -> count |> processDigits digitsRest (digitIndex + 1) // 1
            | 8 :: digitsRest when digitIndex % 2 = 1 -> count |> processDigits digitsRest (digitIndex + 1) // 0
            | 9 :: _ when digitIndex % 2 = 1 -> 0
            | _ -> failwith "Unexpected branch of match expression"
        match digitsCount % 2 with
        | 0 -> 0
        | 1 ->
            // middle digits 0 - 0, 1 - 1, 2 - 2, 3 - 3, 4 - 4
            seq {rangeStart .. rangeFinish} |> Seq.map (fun number -> NumbersDigits.GetFixedSizeDigits(number, digitsCount)) |> Seq.map (fun digits -> 2 * 5 * processDigits digits 0 1) |> Seq.sum
        | _ -> failwith "Unexpected branch of match expression"

    // Notes:
    // 1) Direct calculation of sum n + reverse(n) and its check is slow
    // 2) Instead we will calculate count of possible pairs of reversible numbers
    //    Requirement: all considered numbers must be greater than the current number
    // 2a) Let the current number consists of even count of digits.
    //     Let the current number has the following view: a(0), .., a(m-1), a(m), .., a(n), where a0, ..., an - digits, a(m-1), a(m) - digits at the middle
    //     We can show the following:
    //     a(n) > a(0),
    //     a(n) != 0,
    //     sum of the each pair of digits must be odd and without carry bit (i.e, sum of the each pair of digits = 1, 3, 5, 7, 9).
    //     For calculation of count of the all possible variants, it is enough to use digits a(m), .., a(n) only
    //     Example:
    //     Let, digits a(m), .., a(n) are equal to 625 (digits a(3), a(4), a(5)), then a(0) = 2, 4, a(1) = 1, 3, 5, 7 and a(2) = 1, 3.
    //     Total count of pairs, which are started on digits 526, will be the following: 2 * 4 * 2 = 16.
    //     Thus, total count of pairs of the reversible numbers will be 16; this is means, that total count of the reversible numbers will be 2 * 16 = 32.
    // 2b) Let the current number consists of odd count of digits.
    //     Let the current number has the following view: a(0), .., a(m), .., a(n), where a0, ..., an - digits, a(m) - digit at the middle
    //     We can show that such numbers can be reversible only if count of digits a(0), .., a(m-1) (and a(m+1), .., a(n) correspondingly) is odd
    //     We can show the following:
    //     a(n) > a(0),
    //     a(n) != 0,
    //     sum of the each pair of digits with even index (0, 2, ... n) must be odd with carry bit (i.e sum of the each pair of digits with even index = 11, 13, 15, 17),
    //     sum of the each pair of digits with odd index  (1, 3 ... n - 1) must be even without carry bit and with taking into account of presence carry bit from the younger digit
    //     (i.e, sum of the each pair of digits with odd index = 0, 2, 4, 6, 8),
    //     a(m) makes pair with itself, so (with taking into account of the last rule)  a(m) = 0, 1, 2, 3, 4.
    //     For calculation of count of the all possible variants, it is enough to use digits a(m+1), .., a(n) only
    //     Example:
    //     Let, digits a(m+1), .., a(n) are equal to 628 (digits a(4), a(5), a(6)), then a(0) = 3, 5, 7, a(1) = 0, 2, 4, 6, a(2) = 5, 7, 9 and a(3) = 0, 1, 2, 3, 4
    //     Total count of pairs, which are started on digits 826, will be the following: 3 * 4 * 3 * 5 = 180.
    //     Thus, total count of pairs of the reversible numbers will be 180; this is means, that total count of the reversible numbers will be 2 * 180 = 360.
    let solveImpl (maxNumber: int) =
        // results:
        // result for range [10, 100) is 20 (numbers are 2 digits)
        // result for range [100, 1000) is 100 (numbers are 3 digits)
        // result for range [1000, 10000) is 600 (numbers are 4 digits)
        // result for range [10000, 100000) is 0 (numbers are 5 digits)
        // result for range [100000, 1000000) is 18000 (numbers are 6 digits)
        // result for range [1000000, 10000000) is 50000 (numbers are 7 digits)
        // result for range [10000000, 100000000) is 540000 (numbers are 8 digits)
        // result for range [100000000, 1000000000) is 0 (numbers are 9 digits)
        let rec calcReversibleNumberCount (rangeFinish: int) (startNumber: int) (totalDigitsCount: int) (count: int) =
            match startNumber with
            | _ when startNumber >= maxNumber -> count
            | _ ->
                match totalDigitsCount % 2 with
                | 0 -> count + (calcForEvenDigitsHalfNumberRange 0 (rangeFinish - 1) (totalDigitsCount / 2)) |> calcReversibleNumberCount rangeFinish (10 * startNumber) (totalDigitsCount + 1)
                | 1 -> count + (calcForOddDigitsHalfNumberRange 0 (rangeFinish - 1) (totalDigitsCount / 2)) |> calcReversibleNumberCount (10 * rangeFinish) (10 * startNumber) (totalDigitsCount + 1)
                | _ -> failwith "Unexpected branch of match expression"
        let valueLess100 = calcForLess100 ()
        if maxNumber = 100 then
            valueLess100
        else
            valueLess100 + calcReversibleNumberCount 10 100 3 0

    [<TestCase(1000, 120, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000000, 608720, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)