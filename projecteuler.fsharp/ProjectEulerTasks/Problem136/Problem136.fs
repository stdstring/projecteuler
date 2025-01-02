namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem136() =

    // Brief description:
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

    let rec processSquareDiff (maxNumber: int) (p: int) (q: int) (numberResult: int array) =
        match (2 * p - 1) <= maxNumber with
        | true ->
            let n = (p + q) * (p - q)
            match n > maxNumber with
            | true ->
                numberResult |> processSquareDiff maxNumber (p + 2) (p + 2 - 1)
            | false ->
                let b = p / 2
                numberResult[n] <- numberResult[n] + (if (q < b) && (q > 0) then 2 else 1)
                match q with
                | 0 ->
                    numberResult |> processSquareDiff maxNumber (p + 2) (p + 2 - 1)
                | _ ->
                    numberResult |> processSquareDiff maxNumber p (q - 1)
        | false -> ()

    let solveImpl (maxNumber: int) =
        let numberResult = maxNumber + 1 |> Array.zeroCreate
        numberResult |> processSquareDiff maxNumber 2 1
        numberResult |> Seq.filter (fun value -> value = 1) |> Seq.length

    [<TestCase(20, 8, TimeThresholds.HardTimeLimit)>]
    [<TestCase(32, 11, TimeThresholds.HardTimeLimit)>]
    [<TestCase(33, 11, TimeThresholds.HardTimeLimit)>]
    [<TestCase(99, 25, TimeThresholds.HardTimeLimit)>]
    [<TestCase(49999999, 2544559, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)