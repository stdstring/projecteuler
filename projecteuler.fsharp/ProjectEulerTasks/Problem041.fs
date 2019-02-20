namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System

// We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
// For example, 2143 is a 4-digit pandigital and is also prime. What is the largest n-digit pandigital prime that exists?

[<TestFixture>]
type Problem041() =

    let solveImpl () =
        // solution
        // 1) 10-digits pandigital number contains 0..9 digits. Their sum = 45. 45 is divisible by 3 => all 10-digits pandigital numbers is divisable by 3
        // 2) 9-digits pandigital number contains 1..9 digits. Their sum = 45. 45 is divisible by 3 => all 9-digits pandigital numbers is divisable by 3
        // 3) 8-digits pandigital number contains 1..8 digits. Their sum = 36. 36 is divisible by 3 => all 8-digits pandigital numbers is divisable by 3
        // 4) 7-digits pandigital number contains 1..7 digits. Their sum = 28. 28 is not divisible by 3 => all 7-digits pandigital numbers is not divisable by 3
        // 5) 6-digits pandigital number contains 1..6 digits. Their sum = 21. 21 is divisible by 3 => all 6-digits pandigital numbers is divisable by 3
        // 6) 5-digits pandigital number contains 1..5 digits. Their sum = 15. 15 is divisible by 3 => all 5-digits pandigital numbers is divisable by 3
        // 7) 4-digits pandigital number contains 1..4 digits. Their sum = 10. 10 is not divisible by 3 => all 4-digits pandigital numbers is not divisable by 3
        // So we find max prime pandigital number in 1234567 .. 7654321 and 2143 .. 4321 ranges (because 2143 is prime)
        let maxNumber = 7654321
        let sieveBuilder = EratosSieveBuilder()
        let sieve = sieveBuilder.CreateSieve(maxNumber)
        let rec processPandigitals (lexicographicalNumber: bigint) (digits: int list) =
            let number = Permutations.GetPermutation(lexicographicalNumber, digits) |> NumbersDigits.GetNumber
            match lexicographicalNumber, number |> sieve.IsPrime with
            | _, true -> number |> int |> Some
            | _ when lexicographicalNumber = 0I -> None
            | _ -> processPandigitals (lexicographicalNumber - 1I) digits
        let digits7 = [1 .. 7]
        let digits4 = [1 .. 4]
        match digits7 |> processPandigitals (Permutations.GetLexicographicalNumberSup(digits7) - 1I) with
        | Some number -> number
        | None ->
            match digits4 |> processPandigitals (Permutations.GetLexicographicalNumberSup(digits4) - 1I) with
            | Some number -> number
            | None -> raise (InvalidOperationException())

    [<TestCase(7652413, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl ())