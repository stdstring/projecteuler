namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.IO

module Problem089Impl =
    type NumberData = {Thousands: int; Hundreds: int; Tens: int; Ones: int}

open Problem089Impl

[<TestFixture>]
type Problem089() =

    let generateRomanOnes = function
    | 0 -> ""
    | 1 -> "I"
    | 2 -> "II"
    | 3 -> "III"
    | 4 -> "IV"
    | 5 -> "V"
    | 6 -> "VI"
    | 7 -> "VII"
    | 8 -> "VIII"
    | 9 -> "IX"
    | _ -> failwith "Unexpected branch of match expression"

    let generateRomanTens = function
    | 0 -> ""
    | 1 -> "X"
    | 2 -> "XX"
    | 3 -> "XXX"
    | 4 -> "XL"
    | 5 -> "L"
    | 6 -> "LX"
    | 7 -> "LXX"
    | 8 -> "LXXX"
    | 9 -> "XC"
    | _ -> failwith "Unexpected branch of match expression"

    let generateRomanHundreds = function
    | 0 -> ""
    | 1 -> "C"
    | 2 -> "CC"
    | 3 -> "CCC"
    | 4 -> "CD"
    | 5 -> "D"
    | 6 -> "DC"
    | 7 -> "DCC"
    | 8 -> "DCCC"
    | 9 -> "CM"
    | _ -> failwith "Unexpected branch of match expression"

    let generateRomanThousands (thousands: int) = String.replicate thousands "M"

    let generateRoman (numberData: NumberData) =
        [numberData.Thousands |> generateRomanThousands;
         numberData.Hundreds |> generateRomanHundreds;
         numberData.Tens |> generateRomanTens;
         numberData.Ones |> generateRomanOnes] |> String.concat ""

    let parseSourceRoman (source: string) =
        let normalizeNumber (source: NumberData) =
            let fixedOnes = {source with NumberData.Ones = source.Ones % 10; NumberData.Tens = source.Tens + source.Ones / 10}
            let fixedTens = {fixedOnes with NumberData.Tens = fixedOnes.Tens % 10; NumberData.Hundreds = fixedOnes.Hundreds + fixedOnes.Tens / 10}
            {fixedTens with NumberData.Hundreds = fixedTens.Hundreds % 10; NumberData.Thousands = fixedTens.Thousands + fixedTens.Hundreds / 10}
        let rec parseSourceRomanImpl (sourceSeq: char list) (numberData: NumberData) =
            match sourceSeq with
            | [] -> numberData |> normalizeNumber
            | 'M' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Thousands = numberData.Thousands + 1}
            | 'C' :: 'M' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Hundreds = numberData.Hundreds + 9}
            | 'D' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Hundreds = numberData.Hundreds + 5}
            | 'C' :: 'D' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Hundreds = numberData.Hundreds + 4}
            | 'C' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Hundreds = numberData.Hundreds + 1}
            | 'X' :: 'C' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Tens = numberData.Tens + 9}
            | 'L' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Tens = numberData.Tens + 5}
            | 'X' :: 'L' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Tens = numberData.Tens + 4}
            | 'X' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Tens = numberData.Tens + 1}
            | 'I' :: 'X' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Ones = numberData.Ones + 9}
            | 'V' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Ones = numberData.Ones + 5}
            | 'I' :: 'V' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Ones = numberData.Ones + 4}
            | 'I' :: sourceSeqRest -> parseSourceRomanImpl sourceSeqRest {numberData with NumberData.Ones = numberData.Ones + 1}
            | _ -> failwith "Unexpected branch of match expression"
        parseSourceRomanImpl (source |> Seq.toList) {NumberData.Thousands = 0; NumberData.Hundreds = 0; NumberData.Tens = 0; NumberData.Ones = 0}

    let solveImpl (dataFilename: string) =
        // M -> 1000
        // CM -> 900
        // D -> 500
        // CD -> 400
        // C -> 100
        // XC -> 90
        // L -> 50
        // XL -> 40
        // X -> 10
        // IX -> 9
        // V -> 5
        // IV -> 4
        // I -> 1
        let simplifyNumber (source: string) = source |> parseSourceRoman |> generateRoman
        File.ReadAllLines(Path.Combine("Data", dataFilename)) |> Seq.map (fun source -> source.Length - (source |> simplifyNumber |> String.length)) |> Seq.sum

    [<TestCase("problem_089.dat", 743, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename)