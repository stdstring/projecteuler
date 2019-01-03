namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// It is well known that if the square root of a natural number is not an integer, then it is irrational.
// The decimal expansion of such square roots is infinite without any repeating pattern at all.
// The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.
// For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.

[<TestFixture>]
type Problem080() =

    let pStart = 0I
    let cStart = 0I

    let rec findDigit (p: bigint) (c: bigint) (digit: bigint) =
        match digit with
        | _ when digit = 9I -> 9I
        | _ ->
            let value = digit * (20I * p + digit)
            let nextValue = (digit + 1I) * (20I * p + (digit + 1I))
            match value, nextValue with
            | _ when (value = c) -> digit
            | _ when (value < c) && (nextValue > c) -> digit
            | _ -> (digit + 1I) |> findDigit p c

    let calcNextDigit (p: bigint) (c: bigint) (digit1: int) (digit2: int) =
        let y = c * 100I + (digit1 |> bigint) * 10I + (digit2 |> bigint)
        let digit = findDigit p y 0I
        let newC = y - digit * (20I * p + digit)
        let newP = p * 10I + digit
        (digit |> int), newP, newC

    let calcDigits (integerPart: int list) (digitsCount: int) =
        let rec calcDigitsImpl (sourceIntegerPart: int list) (digitsCountRest: int) (destIntegerPart: int list) (destFractionPart: int list) (p: bigint) (c: bigint) =
            match sourceIntegerPart, digitsCountRest with
            | [], 0 -> (destIntegerPart |> List.rev), (destFractionPart |> List.rev)
            | [], _ ->
                let digit, newP, newC = calcNextDigit p c 0 0
                calcDigitsImpl [] (digitsCountRest - 1) destIntegerPart (digit :: destFractionPart) newP newC
            | digit1 :: digit2 ::sourceIntegerPartRest, _ ->
                let digit, newP, newC = calcNextDigit p c digit1 digit2
                calcDigitsImpl sourceIntegerPartRest (digitsCountRest - 1) (digit :: destIntegerPart) destFractionPart newP newC
            | _ -> failwith "Unexpected branch of match expression"
        calcDigitsImpl integerPart digitsCount [] [] pStart cStart

    let prepareIntegerPartDigits (number: int) =
        match number |> NumbersDigits.GetDigits with
        | digits when digits.Length % 2 = 1 -> 0 :: digits
        | digits -> digits

    let solveImpl (maxNumber: int) (digitCount: int) =
        // TODO (std_string) : probably, move this algorithm into CommonLib
        // Algorithm (from https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Digit-by-digit_calculation):
        // Write the original number in decimal form. The numbers are written similar to the long division algorithm, and, as in long division, the root will be written on the line above.
        // Now separate the digits into pairs, starting from the decimal point and going both left and right. The decimal point of the root will be above the decimal point of the square.
        // One digit of the root will appear above each pair of digits of the square.
        // Beginning with the left-most pair of digits, do the following procedure for each pair:
        // 1) Starting on the left, bring down the most significant (leftmost) pair of digits not yet used (if all the digits have been used, write "00") and
        // write them to the right of the remainder from the previous step (on the first step, there will be no remainder). In other words, multiply the remainder by 100 and add the two digits.
        // This will be the current value c.
        // 2) Find p, y and x, as follows:
        // 2a) Let p be the part of the root found so far, ignoring any decimal point. (For the first step, p = 0).
        // 2b) Determine the greatest digit x such that x(20p + x) <= c. We will use a new variable y = x(20p + x).
        //     Note: 20p + x is simply twice p, with the digit x appended to the right).
        //     Note: You can find x by guessing what c/(20·p) is and doing a trial calculation of y, then adjusting x upward or downward as necessary.
        // 2c) Place the digit x as the next digit of the root, i.e., above the two digits of the square you just brought down. Thus the next p will be the old p times 10 plus x.
        // 3) Subtract y from c to form a new remainder.
        // 4) If the remainder is zero and there are no more digits to bring down, then the algorithm has terminated. Otherwise go back to step 1 for another iteration.

        let squares = seq { 2 .. maxNumber } |> Seq.map (fun number -> number * number) |> Seq.takeWhile (fun number -> number <= maxNumber) |> HashSet<int>
        seq { 2 .. maxNumber } |>
        Seq.filter (fun number -> number |> squares.Contains |> not) |>
        Seq.map (fun number -> let integerPart, fractionPart = calcDigits (number |> prepareIntegerPartDigits) digitCount in (integerPart |> List.sum) + (fractionPart |> List.sum)) |>
        Seq.sum

    [<TestCase(2, 100, 475, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 100, 40886, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, digitCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, digitCount)