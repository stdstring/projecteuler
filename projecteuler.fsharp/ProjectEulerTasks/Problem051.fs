namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// By replacing the 1-st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
// By replacing the 3-rd and 4-th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers,
// yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.
// Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

module Problem051Impl =

    type DigitInfo = {Digit: int; Positions: int list}

    type DigitsInfo = {Info: DigitInfo[]; ElderPos: int}

open Problem051Impl

[<TestFixture>]
type Problem051() =

    // TODO (std_string) : think about selection of this value
    [<Literal>]
    let MaxNumber = 9999999

    // TODO (std_string) : think about selection of this value
    [<Literal>]
    let MaxDigitsCount = 7

    let digitsWeight = seq {0 .. MaxDigitsCount - 1} |> Seq.map (fun digitPos -> pown 10 digitPos) |> Seq.toArray

    let createDigitsInfo (number: int) =
        let digitsCount = 10
        let digitsInfo = Array.create digitsCount []
        let digits = number |> NumbersDigits.GetDigits
        digits |> Seq.rev |> Seq.iteri (fun pos digit -> digitsInfo.[digit] <- pos :: digitsInfo.[digit])
        {DigitsInfo.Info = seq {0 .. 9} |> Seq.map (fun digit -> {DigitInfo.Digit = digit; DigitInfo.Positions = digitsInfo.[digit]}) |> Seq.toArray; DigitsInfo.ElderPos = digits.Length - 1}

    let createDigitPart (digitInfo: DigitInfo) =
        digitInfo.Positions |> Seq.map (fun pos -> digitsWeight.[pos]) |> Seq.sum |> (*) digitInfo.Digit

    let createNumberBasis (excludedDigit: int) (digitsInfo: DigitsInfo) =
        seq {0 .. 9} |> Seq.filter (fun digit -> digit <> excludedDigit) |> Seq.map (fun digit -> digitsInfo.Info.[digit] |> createDigitPart) |> Seq.sum

    let createNumber (numberBasis: int) (variantDigitInfo: DigitInfo) =
        numberBasis + (variantDigitInfo |> createDigitPart)

    let checkPossibleVariant (familySize: int) (elderPos: int) (digitInfo: DigitInfo) =
        match digitInfo.Digit, digitInfo.Positions with
        | _, [] -> false
        // last (younger) digit must be 1, 3, 7, 9
        | 1, 0 :: _ -> familySize <= 4
        | 3, 0 :: _ -> familySize <= 3
        | 7, 0 :: _ -> familySize <= 2
        | 9, 0 :: _ -> familySize = 1
        | _, 0 :: _ -> failwith "Unexpected branch of match expression"
        // elder position mustn't contain 0
        | digit, positions when positions |> List.contains elderPos -> familySize <= (10 - digit - 1)
        | digit, _ -> familySize <= (10 - digit)

    let getPossibleVariantDigits (elderPos: int) (digitInfo: DigitInfo) =
        match digitInfo.Digit, digitInfo.Positions with
        // last (younger) digit must be 1, 3, 7, 9
        | 1, 0 :: _ -> [1; 3; 7; 9]
        | 3, 0 :: _ -> [3; 7; 9]
        | 7, 0 :: _ -> [7; 9]
        | 9, 0 :: _ -> [9]
        // elder position mustn't contain 0
        | 0, positions when positions |> List.contains elderPos -> [1 .. 9]
        | digit, _ -> [digit .. 9]

    let getPossibleVariants (familySize: int) (digitsInfo : DigitsInfo) =
        digitsInfo.Info |> Seq.filter (fun digitInfo -> digitInfo |> checkPossibleVariant familySize digitsInfo.ElderPos) |> Seq.toList

    let checkPossibleVariant (familySize: int) (sieve: EratosSieve) (digitsInfo: DigitsInfo) (variant : DigitInfo) =
        let numberBasis = digitsInfo |> createNumberBasis variant.Digit
        let rec checkPossibleVariantImpl (suitableVariants: int) (digits: int list) =
            match digits with
            | [] -> false
            | digit :: digitsRest ->
                let number = {variant with DigitInfo.Digit = digit} |> createNumber numberBasis
                match sieve.IsPrime(number) with
                | false -> digitsRest |> checkPossibleVariantImpl suitableVariants
                | true when suitableVariants = (familySize - 1) -> true
                | true -> digitsRest |> checkPossibleVariantImpl (suitableVariants + 1)
        variant |> getPossibleVariantDigits digitsInfo.ElderPos |> checkPossibleVariantImpl 0

    let processNumber (familySize: int) (sieve: EratosSieve) (number: int) =
        let digitsInfo = number |> createDigitsInfo
        digitsInfo |> getPossibleVariants familySize |> Seq.exists (fun variant -> variant |> checkPossibleVariant familySize sieve digitsInfo)

    let solveImpl (familySize: int) =
        let sieve = EratosSieve.Create(MaxNumber)
        // we don't process single digit primes
        sieve.ToSeq() |> Seq.skipWhile (fun prime -> prime < 10) |> Seq.find (fun prime -> prime |> processNumber familySize sieve)

    [<TestCase(6, 13, TimeThresholds.HardTimeLimit)>]
    [<TestCase(7, 56003, TimeThresholds.HardTimeLimit)>]
    [<TestCase(8, 121313, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(familySize: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, familySize)
