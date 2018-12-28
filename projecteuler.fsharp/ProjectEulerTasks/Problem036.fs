namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The decimal number, 585 (base 10) = 1001001001 (base 2), is palindromic in both bases.
// Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
// (Please note that the palindromic number, in either base, may not include leading zeros.)

module Problem036Impl =
    // Digits allways bottom half of number's digits, e.g, for number 67576 Digits = [5, 7, 6], for number 675576 Digits = [5, 7, 6]
    type NumberData = {Digits: List<int>; DigitsCount: int}

open Problem036Impl

[<TestFixture>]
type Problem036() =

    let getNumber (data: NumberData) =
        let digits = match data.DigitsCount with
                     | 1 -> data.Digits
                     | _ when data.DigitsCount % 2 = 0 -> List.append (data.Digits |> List.rev) data.Digits
                     | _ -> List.append (data.Digits |> List.tail |> List.rev) data.Digits
        digits |> NumbersDigits.GetNumber |> int

    let getNextNumberData (data: NumberData) =
        let rec getNextNumberDataImpl (source: List<int>) (dest: List<int>) =
            match source with
            | [9] when data.DigitsCount % 2 = 0 -> {NumberData.Digits = 0 :: dest @ [1]; DigitsCount = data.DigitsCount + 1}
            | [9] when data.DigitsCount % 2 = 1 -> {NumberData.Digits = dest @ [1]; DigitsCount = data.DigitsCount + 1}
            | 9 :: digitsRest -> getNextNumberDataImpl digitsRest (dest @ [0])
            | digit :: digitsRest -> {NumberData.Digits = dest @ [digit + 1] @ digitsRest; DigitsCount = data.DigitsCount}
            | [] -> failwith "Unexpected branch of match expression"
        getNextNumberDataImpl data.Digits []

    let solveImpl (maxNumber: int) =
        {NumberData.Digits = [1]; DigitsCount = 1} |>
        Seq.unfold (fun data -> Some (data |> getNumber, data |> getNextNumberData)) |>
        Seq.takeWhile (fun number -> number <= maxNumber) |>
        Seq.filter (fun number -> let binDigits = NumbersDigits.GetDigits(number, 2) in binDigits = (binDigits |> List.rev)) |>
        Seq.sum

    [<TestCase(999999, 872187, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl maxNumber)