namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem135() =

    // Brief description:
    //
    // approach 1:
    // x^2 - y^2 - z^2 = n^2
    // let z = a, y = a + d, x = a + 2d
    // (a + 2d)^2 - (a + d)^2 - a^2 = n
    // (a^2 + 4ad + 4d^2) - (a^2 + 2ad + d^2) - a^2 = n
    // -a^2 + 2ad + 3d^2 = -(a^2 - 2ad - 3d^2) = -(a + d) * (a - 3d) = (a + d) * (3d - a)
    // (a + d) * (3d - a) = n = F1 * F2 => (a + d) = F1, (3d - a) = F2
    // d = (F1 + F2) / 4, a = (3F1 - F2) / 4 => (F1 + F2) % 4 == 0, (3F1 - F2) % 4 == 0, 3F1 > F2
    //
    // approach 2:
    // x^2 - y^2 - z^2 = n^2
    // let z = a, y = a + d, x = a + 2d
    // (a + 2d)^2 - (a + d)^2 - a^2 = n
    // (a^2 + 4ad + 4d^2) - (a^2 + 2ad + d^2) - a^2 = n
    // -a^2 + 2ad + 3d^2 = n
    // -a^2 + 2ad + 3d^2 = -a^2 + 2ad + 3d^2 + d^2 - d^2 = 4d^2 - (a^2 - 2ad + d^2) = 4d^2 - (a - d)^2
    // 4d^2 - (a - d)^2 = (2d)^2 - (a - d)^2 = n
    // let P^2 = (2d)^2, Q^2 = (a - d)^2 = > P^2 - Q^2 = n
    // Q = |a - b| => Q = (a - b) || Q = (b - a)
    // if Q = 0 => a = b || b = a - 1 case (2 equivalent cases)
    // if Q > b => a - b > b || b - a > b => a > 2b || a < 0 - 1 case
    // if Q < b => a - b < b || b - a < b => a < 2b || a > 0 - 2 cases

    let calcSolutionCountForFactors (factor1: int) (factor2: int) =
        match (factor1 + factor2) &&& 0b11 with
        | 0 ->
            let delta1 = 3 * factor1 - factor2
            let delta2 = 3 * factor2 - factor1
            let a1 = match (delta1 > 0) && ((delta1 &&& 0b11) = 0) with
                     | true -> delta1 / 4
                     | false -> 0
            let a2 = match (delta2 > 0) && ((delta2 &&& 0b11) = 0) with
                     | true -> delta2 / 4
                     | false -> 0
            (if a1 > 0 then 1 else 0) + (if (a2 > 0) && (a2 <> a1) then 1 else 0)
        | _ -> 0

    let calcSolutionCountForNumber (number: int) =
        let start = if (number &&& 0b1) = 0 then 2 else 1
        let delta = if (number &&& 0b1) = 0 then 2 else 1
        let mutable result = 0
        let mutable factor = start
        while factor * factor <= number do
            if number % factor = 0 then
                result <- result + calcSolutionCountForFactors factor (number / factor)
            factor <- factor + delta
        result

    let solveViaFactorizationImpl (maxNumber: int) (solutionCount: int) =
        let mutable result = 0
        for number in seq {1 .. maxNumber} do
            let countForNumber = number |> calcSolutionCountForNumber
            if countForNumber = solutionCount then
                result <- result + 1
        result

    let solveViaDirectCalculationImpl (maxNumber: int) (solutionCount: int) =
        let numberResult = maxNumber + 1 |> Array.zeroCreate
        for i = 1 to maxNumber do
            let mutable j = i
            while j <= maxNumber do
                if (((i + (j / i)) &&& 0b11) = 0) &&
                   (((3 * i - (j / i)) &&& 0b11) = 0) &&
                   (3 * i > (j/i)) then
                   numberResult[j] <- numberResult[j] + 1
                j <- j + i
        numberResult |> Seq.filter (fun value -> value = solutionCount) |> Seq.length

    let rec processSquareDiffCalculation (maxNumber: int) (p: int) (q: int) (numberResult: int array) =
        match (2 * p - 1) <= maxNumber with
        | true ->
            let n = (p + q) * (p - q)
            match n > maxNumber with
            | true ->
                numberResult |> processSquareDiffCalculation maxNumber (p + 2) (p + 2 - 1)
            | false ->
                let b = p / 2
                numberResult[n] <- numberResult[n] + (if (q < b) && (q > 0) then 2 else 1)
                match q with
                | 0 ->
                    numberResult |> processSquareDiffCalculation maxNumber (p + 2) (p + 2 - 1)
                | _ ->
                    numberResult |> processSquareDiffCalculation maxNumber p (q - 1)
        | false -> ()

    let solveViaSquareDiffCalculationImpl (maxNumber: int) (solutionCount: int) =
        let numberResult = maxNumber + 1 |> Array.zeroCreate
        numberResult |> processSquareDiffCalculation maxNumber 2 1
        numberResult |> Seq.filter (fun value -> value = solutionCount) |> Seq.length

    [<TestCase(27, 2, 1, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1155, 10, 1, TimeThresholds.HardTimeLimit)>]
    [<TestCase(999999, 10, 4989, TimeThresholds.HardTimeLimit)>]
    member public this.SolveViaFactorization(maxNumber: int, solutionCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveViaFactorizationImpl, maxNumber, solutionCount)

    [<TestCase(27, 2, 1, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1155, 10, 1, TimeThresholds.HardTimeLimit)>]
    [<TestCase(999999, 10, 4989, TimeThresholds.HardTimeLimit)>]
    member public this.SolveViaDirectCalculation(maxNumber: int, solutionCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveViaDirectCalculationImpl, maxNumber, solutionCount)

    [<TestCase(27, 2, 1, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1155, 10, 1, TimeThresholds.HardTimeLimit)>]
    [<TestCase(999999, 10, 4989, TimeThresholds.HardTimeLimit)>]
    member public this.SolveViaSquareDiffCalculation(maxNumber: int, solutionCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveViaSquareDiffCalculationImpl, maxNumber, solutionCount)