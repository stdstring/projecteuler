namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem032() =

    let alphabet = [1 .. 9]

    let getAlphabetRest (alphabet:  int list) (permutation: int list) =
        alphabet |> List.filter (fun digit -> permutation |> List.contains digit |> not)

    let checkFactors (result: int) (firstFactor: int) (secondFactorDigits: int list) =
        match secondFactorDigits with
        | [digit] -> firstFactor * digit = result
        | [digit1; digit2] ->
            let secondFactor1 = digit1 * 10 + digit2
            let secondFactor2 = digit2 * 10 + digit1
            (firstFactor * secondFactor1 = result) || (firstFactor * secondFactor2 = result)
        | _ -> failwith "Unexpected branch of match expression"

    let checkResult (result: int) (resultDigits: int list) =
        let alphabetRest = resultDigits |> getAlphabetRest alphabet
        let rec processFactor (lexicographicalNumber: bigint) (lexicographicalNumberSup: bigint) (size: int) =
            match lexicographicalNumber with
            | _ when lexicographicalNumber = lexicographicalNumberSup -> false
            | _ ->
                let factorDigits = Permutations.GetPermutation(lexicographicalNumber, size, alphabetRest)
                let factor = factorDigits |> NumbersDigits.GetNumber |> int
                match factorDigits |> getAlphabetRest alphabetRest |> checkFactors result factor with
                | false -> processFactor (lexicographicalNumber + 1I) lexicographicalNumberSup size
                | true -> true
        (processFactor 0I (Permutations.GetLexicographicalNumberSup(alphabetRest, 3)) 3) ||
        (processFactor 0I (Permutations.GetLexicographicalNumberSup(alphabetRest, 4)) 4)

    let solveImpl () =
        // factorA * factorB = result
        // possible sizes : factorA = 3 digits, factor2 = 2 digits, result = 4 digits; factorA = 4 digits, factor2 = 1 digits, result = 4 digits => result always is 4 digits
        let mutable sum = 0
        let resultSize = 4
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(alphabet, resultSize)
        for lexicographicalNumber in seq {0I .. lexicographicalNumberSup - 1I} do
            let resultDigits = Permutations.GetPermutation(lexicographicalNumber, resultSize, alphabet)
            let result = resultDigits |> NumbersDigits.GetNumber |> int
            if (resultDigits |> checkResult result) then
                sum <- sum + result
        sum

    [<TestCase(45228, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl ())