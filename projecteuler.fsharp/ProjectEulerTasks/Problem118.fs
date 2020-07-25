namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// Using all of the digits 1 through 9 and concatenating them freely to form decimal integers, different sets can be formed.
// Interestingly with the set {2,5,47,89,631}, all of the elements belonging to it are prime.
// How many distinct sets containing each of the digits one through nine exactly once contain only prime elements?

[<TestFixture>]
type Problem118() =

    // 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 = 45, 45 mod 3 = 0 => pandigital number 1 .. 9 can't be prime number => max prime < 99999999 => max prime size = 8
    [<Literal>]
    let MaxNumber = 99999999

    [<Literal>]
    let MaxPrimeSize = 8

    let createDigitsUsage (number: int) =
        let rec createDigitsUsageImpl (numberRest: int) (digitUsage: int) =
            match numberRest with
            | 0 -> digitUsage
            | _ ->
                match numberRest % 10 with
                | 0 -> 0
                | digit when digitUsage &&& (1 <<< (digit - 1)) > 0 -> 0
                | digit -> digitUsage ||| (1 <<< (digit - 1)) |> createDigitsUsageImpl (numberRest / 10)
        createDigitsUsageImpl number 0

    let generateSuitablePrimes () =
        let primes = (MaxNumber |> CommonLib.EratosSieve.Create).ToSeq()
        let suitablePrimes = Array.zeroCreate (MaxPrimeSize + 1)
        let mutable size  = 1
        let mutable border = 10
        suitablePrimes.[size] <- new ResizeArray<int>()
        for prime in primes do
            if prime > border then
                size <- size + 1
                border <- border * 10
                suitablePrimes.[size] <- new ResizeArray<int>()
            match prime |> createDigitsUsage with
            | 0 -> ()
            | digitsUsage -> digitsUsage |> suitablePrimes.[size].Add
        suitablePrimes

    let generateAACombinations (numbers: ResizeArray<int>) =
        let combinations = new ResizeArray<int>()
        for index1 in 0 .. numbers.Count - 2 do
            for index2 in index1 + 1 .. numbers.Count - 1 do
                let number1 = numbers.[index1]
                let number2 = numbers.[index2]
                if (number1 &&& number2 = 0) then
                    number1 ||| number2 |> combinations.Add
        combinations

    let generateABCombinations (numbers1: ResizeArray<int>) (numbers2: ResizeArray<int>) =
        let combinations = new ResizeArray<int>()
        for number1 in numbers1 do
            for number2 in numbers2 do
                if (number1 &&& number2 = 0) then
                    number1 ||| number2 |> combinations.Add
        combinations

    let generateAAACombinations (numbers: ResizeArray<int>) =
        let combinations = new ResizeArray<int>()
        for index1 in 0 .. numbers.Count - 3 do
            for index2 in index1 + 1 .. numbers.Count - 2 do
                for index3 in index2 + 1 .. numbers.Count - 1 do
                    let number1 = numbers.[index1]
                    let number2 = numbers.[index2]
                    let number3 = numbers.[index3]
                    if (number1 &&& number2 = 0) && (number1 &&& number3 = 0) && (number2 &&& number3 = 0) then
                        number1 ||| number2 ||| number3 |> combinations.Add
        combinations

    let calcACombinations (numbers: ResizeArray<int>) (combination: int) (result: int) =
        let mutable count = 0
        for number in numbers do
            if (combination &&& number = 0) then
                count <- count + 1
        result + count

    let calcABCombinations (numbers1: ResizeArray<int>) (numbers2: ResizeArray<int>) (result: int) =
        let mutable count = 0
        for number1 in numbers1 do
            for number2 in numbers2 do
                if (number1 &&& number2 = 0) then
                    count <- count + 1
        result + count

    let calcABCCombinations (numbers1: ResizeArray<int>) (numbers2: ResizeArray<int>) (numbers3: ResizeArray<int>) (result: int) =
        let mutable count = 0
        for number1 in numbers1 do
            for number2 in numbers2 do
                for number3 in numbers3 do
                    if (number1 &&& number2 = 0) && (number1 &&& number3 = 0) && (number2 &&& number3 = 0) then
                        count <- count + 1
        result + count

    let calcAABCombinations (numbers1: ResizeArray<int>) (numbers2: ResizeArray<int>) (result: int) =
        let mutable count = 0
        for index1 in 0 .. numbers1.Count - 2 do
            for index2 in index1 + 1 .. numbers1.Count - 1 do
                for number2 in numbers2 do
                    if (number2 &&& numbers1.[index1] = 0) && (number2 &&& numbers1.[index2] = 0) && (numbers1.[index1] &&& numbers1.[index2] = 0) then
                        count <- count + 1
        result + count

    let calcAAACombinations (numbers: ResizeArray<int>) (result: int) =
        let mutable count = 0
        for index1 in 0 .. numbers.Count - 3 do
            for index2 in index1 + 1 .. numbers.Count - 2 do
                for index3 in index2 + 1 .. numbers.Count - 1 do
                    let number1 = numbers.[index1]
                    let number2 = numbers.[index2]
                    let number3 = numbers.[index3]
                    if (number1 &&& number2 = 0) && (number1 &&& number3 = 0) && (number2 &&& number3 = 0) then
                        count <- count + 1
        result + count

    let calcAAAABCombinations (numbers1: ResizeArray<int>) (numbers2: ResizeArray<int>) (result: int) =
        let mutable count = 0
        for index1 in 0 .. numbers1.Count - 4 do
            for index2 in index1 + 1 .. numbers1.Count - 3 do
                for index3 in index2 + 1 .. numbers1.Count - 2 do
                    for index4 in index3 + 1 .. numbers1.Count - 1 do
                        for number2 in numbers2 do
                            if (numbers1.[index1] &&& numbers1.[index2] = 0) &&
                               (numbers1.[index1] &&& numbers1.[index3] = 0) &&
                               (numbers1.[index1] &&& numbers1.[index4] = 0) &&
                               (numbers1.[index1] &&& number2 = 0) &&
                               (numbers1.[index2] &&& numbers1.[index3] = 0) &&
                               (numbers1.[index2] &&& numbers1.[index4] = 0) &&
                               (numbers1.[index2] &&& number2 = 0) &&
                               (numbers1.[index3] &&& numbers1.[index4] = 0) &&
                               (numbers1.[index3] &&& number2 = 0) &&
                               (numbers1.[index4] &&& number2 = 0) then
                                count <- count + 1
        result + count

    let solveImpl () =
        let primes = generateSuitablePrimes ()
        // 1 + 1
        let combinations11 = primes.[1] |> generateAACombinations
        // 1 + 2
        let combinations12 = generateABCombinations primes.[1] primes.[2]
        // 1 + 1 + 1
        let combinations111 = primes.[1] |> generateAAACombinations
        // 1 + 1 + 2
        let combinations112 = generateABCombinations combinations11 primes.[2]
        // 1 + 1 + 3
        let combinations113 = generateABCombinations combinations11 primes.[3]
        // 1 + 1 + 1 + 1 = {2, 3, 5, 7}
        let combination1111 = primes.[1] |> Seq.sum
        // 2 + 2
        let combinations22 = primes.[2] |> generateAACombinations
        // 2 + 2 + 2
        let combinations222 = primes.[2] |> generateAAACombinations
        // 2 + 3
        let combinations23 = generateABCombinations primes.[2] primes.[3]
        // 3 + 3
        let combinations33 = primes.[3] |> generateAACombinations
        // calc
        0 |>
        calcABCombinations primes.[1] primes.[8] |>                     // 9 = 8 + 1
        calcABCombinations primes.[2] primes.[7] |>                     // 9 = 7 + 2
        calcABCombinations combinations11 primes.[7] |>                 // 9 = 7 + 1 + 1
        calcABCombinations primes.[3] primes.[6] |>                     // 9 = 6 + 3
        calcABCombinations combinations12 primes.[6] |>                 // 9 = 6 + 2 + 1
        calcABCombinations combinations111 primes.[6] |>                // 9 = 6 + 1 + 1 + 1
        calcABCombinations primes.[4] primes.[5] |>                     // 9 = 5 + 4
        calcABCCombinations primes.[1] primes.[3] primes.[5] |>         // 9 = 5 + 3 + 1
        calcABCombinations combinations22 primes.[5] |>                 // 9 = 5 + 2 + 2
        calcABCombinations combinations112 primes.[5] |>                // 9 = 5 + 2 + 1 + 1
        calcACombinations primes.[5] combination1111 |>                 // 9 = 5 + 1 + 1 + 1 + 1
        calcAABCombinations primes.[4] primes.[1] |>                    // 9 = 4 + 4 + 1
        calcABCombinations combinations23 primes.[4] |>                 // 9 = 4 + 3 + 2
        calcABCombinations combinations113 primes.[4] |>                // 9 = 4 + 3 + 1 + 1
        calcABCCombinations primes.[1] primes.[4] combinations22 |>     // 9 = 4 + 2 + 2 + 1
        calcABCCombinations primes.[2] primes.[4] combinations111 |>    // 9 = 4 + 2 + 1 + 1 + 1
        calcAAACombinations primes.[3] |>                               // 9 = 3 + 3 + 3
        calcABCombinations combinations12 combinations33 |>             // 9 = 3 + 3 + 2 + 1
        calcABCombinations combinations111 combinations33 |>            // 9 = 3 + 3 + 1 + 1 + 1
        calcABCombinations combinations222 primes.[3] |>                // 9 = 3 + 2 + 2 + 2
        calcABCCombinations primes.[3] combinations11 combinations22 |> // 9 = 3 + 2 + 2 + 1 + 1
        calcACombinations combinations23 combination1111 |>             // 9 = 3 + 2 + 1 + 1 + 1 + 1
        calcAAAABCombinations primes.[2] primes.[1] |>                  // 9 = 2 + 2 + 2 + 2 + 1
        calcABCombinations combinations111 combinations222              // 9 = 2 + 2 + 2 + 1 + 1 + 1

    [<TestCase(44680, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)