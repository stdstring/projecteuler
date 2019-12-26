namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib.Rational
open CommonLib
open System.Collections.Generic

// By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic operations (+, −, *, /) and brackets/parentheses,
// it is possible to form different positive integer targets.
// For example,
// 8 = (4 * (1 + 3)) / 2
// 14 = 4 * (3 + 1 / 2)
// 19 = 4 * (2 + 3) − 1
// 36 = 3 * 4 * (2 + 1)
// Note that concatenations of the digits, like 12 + 34, are not allowed.
// Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum,
// and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.
// Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 1 to n, can be obtained, giving your answer as a string: abcd.

[<TestFixture>]
type Problem093() =

    let digits = [0 .. 9]

    [<Literal>]
    let DigitsCount = 4

    // TODO (std_string) : use union type for operators
    let operators = [|'+'; '-'; '*'; '/'|]

    [<Literal>]
    let OperatorCount = 3

    let operatorCombinationCount = pown operators.Length OperatorCount

    let storageSize = 6 * 7 * 8 * 9 + 1

    let calc2Value (value1: RationalNumber32 option) (operator: char) (value2: RationalNumber32 option) =
        match value1, operator, value2 with
        | None, _, _ -> None
        | _, _, None -> None
        | Some number1, '+', Some number2 -> number1 + number2 |> Some
        | Some number1, '-', Some number2 -> number1 - number2 |> Some
        | Some number1, '*', Some number2 -> number1 * number2 |> Some
        | _, '/', Some number2 when number2.IsZero -> None
        | Some number1, '/', Some number2 -> number1 / number2 |> Some
        | _ -> failwith "Unexpected branch of match expression"

    let calc3Value (value1: RationalNumber32 option) (operator1: char) (value2: RationalNumber32 option) (operator2: char) (value3: RationalNumber32 option) =
        match operator1, operator2 with
        | '*', _ -> calc2Value (calc2Value value1 '*' value2) operator2 value3
        | '/', _ -> calc2Value (calc2Value value1 '/' value2) operator2 value3
        | _, '*' -> calc2Value value1 operator1 (calc2Value value2 '*' value3)
        | _, '/' -> calc2Value value1 operator1 (calc2Value value2 '/' value3)
        | _ -> calc2Value (calc2Value value1 operator1 value2) operator2 value3

    let calc4Value (value1: RationalNumber32 option) (operator1: char) (value2: RationalNumber32 option) (operator2: char) (value3: RationalNumber32 option) (operator3: char) (value4: RationalNumber32 option) =
        match operator1, operator2, operator3 with
        | '*', _, _ -> calc3Value (calc2Value value1 '*' value2) operator2 value3 operator3 value4
        | '/', _, _ -> calc3Value (calc2Value value1 '/' value2) operator2 value3 operator3 value4
        | _, '*', _ -> calc3Value value1 operator1 (calc2Value value2 '*' value3) operator3 value4
        | _, '/', _ -> calc3Value value1 operator1 (calc2Value value2 '/' value3) operator3 value4
        | _, _, '*' -> calc3Value value1 operator1 value2 operator2 (calc2Value value3 '*' value4)
        | _, _, '/' -> calc3Value value1 operator1 value2 operator2 (calc2Value value3 '/' value4)
        | _ -> calc3Value (calc2Value value1 operator1 value2) operator2 value3 operator3 value4

    let calcDigitsValues ([digit1; digit2; digit3; digit4]: int list) ([operator1; operator2; operator3]: char list) =
        let number1 = new RationalNumber32(digit1) |> Some
        let number2 = new RationalNumber32(digit2) |> Some
        let number3 = new RationalNumber32(digit3) |> Some
        let number4 = new RationalNumber32(digit4) |> Some
        [
            // 1: digit1 operator1 digit2 operator2 digit3 operator3 digit4
            calc4Value number1 operator1 number2 operator2 number3 operator3 number4;
            // 2: (digit1 operator1 digit2) operator2 digit3 operator3 digit4
            calc3Value (calc2Value number1 operator1 number2) operator2 number3 operator3 number4;
            // 3: digit1 operator1 (digit2 operator2 digit3) operator3 digit4
            calc3Value number1 operator1 (calc2Value number2 operator2 number3) operator3 number4;
            // 4: digit1 operator1 digit2 operator2 (digit3 operator3 digit4)
            calc3Value number1 operator1 number2 operator2 (calc2Value number3 operator3 number4);
            // 5: (digit1 operator1 digit2) operator2 (digit3 operator3 digit4)
            calc2Value (calc2Value number1 operator1 number2) operator2 (calc2Value number3 operator3 number4);
            // 6: (digit1 operator1 digit2 operator2 digit3) operator3 digit4 is equivalent to 7 or 8
            //calc2Value (calc3Value number1 operator1 number2 operator2 number3) operator3 number4;
            // 7: ((digit1 operator1 digit2) operator2 digit3) operator3 digit4
            calc2Value (calc2Value (calc2Value number1 operator1 number2) operator2 number3) operator3 number4;
            // 8: (digit1 operator1 (digit2 operator2 digit3)) operator3 digit4
            calc2Value (calc2Value number1 operator1 (calc2Value number2 operator2 number3)) operator3 number4;
            // 9: digit1 operator1 (digit2 operator2 digit3 operator3 digit4) is equivalent to 10 or 11
            //calc2Value number1 operator1 (calc3Value number2 operator2 number3 operator3 number4);
            // 10: digit1 operator1 ((digit2 operator2 digit3) operator3 digit4)
            calc2Value number1 operator1 (calc2Value (calc2Value number2 operator2 number3) operator3 number4);
            // 11: digit1 operator1 (digit2 operator2 (digit3 operator3 digit4))
            calc2Value number1 operator1 (calc2Value number2 operator2 (calc2Value number3 operator3 number4))
        ]

    let collectValues (storage: bool[]) (values: RationalNumber32 option list) =
        let iterFun (value: RationalNumber32 option) =
            match value with
            | None -> ()
            | Some value when value.IsInteger |> not -> ()
            | Some value when value.IsInteger && value.Numerator <= 0 -> ()
            | Some value when value.IsInteger && value.Numerator > 0 -> storage.[(value.Numerator |> int) - 1]<-true
            | _ -> failwith "Unexpected branch of match expression"
        values |> List.iter iterFun

    let calcLength (storage: bool[]) =
        storage |> Seq.takeWhile (fun value -> value) |> Seq.length

    let processDigits (storage: IDictionary<string, bool[]>) (operationCombinationStorage: char list []) (digits: int list) =
        let key = digits |> List.sort |> System.String.Concat
        if key |> storage.ContainsKey |> not then
            storage.Add(key, Array.create storageSize false)
        for combinationIndex in seq {0 .. operatorCombinationCount - 1} do
            operationCombinationStorage.[combinationIndex] |> calcDigitsValues digits |> collectValues storage.[key]

    let generateOperationCombinations () =
        let operationCombinationStorage = Array.create operatorCombinationCount []
        for combinationIndex in seq {0 .. operatorCombinationCount - 1} do
            let combinationDigits = NumbersDigits.GetDigits(combinationIndex, operators.Length)
            operationCombinationStorage.[combinationIndex] <- combinationDigits |> List.append (List.init (OperatorCount - combinationDigits.Length) (fun _ -> 0)) |> List.map (fun digitIndex -> operators.[digitIndex])
        operationCombinationStorage

    let solveImpl () =
        let operationCombinationStorage = generateOperationCombinations ()
        let storage = new Dictionary<string, bool[]>()
        let lexicographicalNumberSup = Permutations.GetLexicographicalNumberSup(digits, DigitsCount)
        for lexicographicalNumber in seq {1I .. lexicographicalNumberSup - 1I} do
            Permutations.GetPermutation(lexicographicalNumber, DigitsCount, digits) |> processDigits storage operationCombinationStorage
        (storage |> Seq.maxBy (fun kvPair -> kvPair.Value |> calcLength)).Key

    [<TestCase("1258", TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: string, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)
