namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.
// Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.
// We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.
// Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525) are bouncy.
// In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.
// Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.
// Find the least number for which the proportion of bouncy numbers is exactly 99%.

module Problem112Impl =

    type DigitsOrdering =
    | Asc of lastDigit: int
    | Desc of lastDigit: int
    | Undef of lastDigit: int

open Problem112Impl
open CommonLib

[<TestFixture>]
type Problem112() =

    let isBouncyNumber (number: int) =
        let rec isBouncyNumberImpl (digits: int list) (ordering: DigitsOrdering) =
            match digits, ordering with
            | [], _ -> false
            | digit :: digitsRest, DigitsOrdering.Asc lastDigit when digit >= lastDigit -> DigitsOrdering.Asc digit |> isBouncyNumberImpl digitsRest
            | digit :: digitsRest, DigitsOrdering.Desc lastDigit when digit <= lastDigit -> DigitsOrdering.Desc digit |> isBouncyNumberImpl digitsRest
            | digit :: digitsRest, DigitsOrdering.Undef lastDigit when digit > lastDigit -> DigitsOrdering.Asc digit |> isBouncyNumberImpl digitsRest
            | digit :: digitsRest, DigitsOrdering.Undef lastDigit when digit < lastDigit -> DigitsOrdering.Desc digit |> isBouncyNumberImpl digitsRest
            | digit :: digitsRest, DigitsOrdering.Undef lastDigit when digit = lastDigit -> DigitsOrdering.Undef digit |> isBouncyNumberImpl digitsRest
            | _ -> true
        match number with
        | _ when number < 100 -> false
        | _ -> let digit :: digitsRest = number |> NumbersDigits.GetDigits in DigitsOrdering.Undef digit |> isBouncyNumberImpl digitsRest

    let solveImpl (bouncyNumbersProportion: float) =
        let rec processNumber (number: int) (bouncyNumbersCount: int) =
            match number |> isBouncyNumber with
            | false -> processNumber (number + 1) bouncyNumbersCount
            | true ->
                let newBouncyNumbersCount = bouncyNumbersCount + 1
                let totalCount = number
                let currentProportion = (newBouncyNumbersCount |> float) / (totalCount |> float)
                match currentProportion with
                | _ when currentProportion >= bouncyNumbersProportion -> number
                | _ -> processNumber (number + 1) newBouncyNumbersCount
        processNumber 100 0

    [<TestCase(0.5, 538, TimeThresholds.HardTimeLimit)>]
    [<TestCase(0.9, 21780, TimeThresholds.HardTimeLimit)>]
    [<TestCase(0.99, 1587000, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(bouncyNumbersProportion: float, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, bouncyNumbersProportion)