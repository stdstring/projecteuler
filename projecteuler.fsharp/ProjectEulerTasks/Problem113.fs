namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.
// Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.
// We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.
// As n increases, the proportion of bouncy numbers below n increases such that there are only 12951 numbers below one-million that are not bouncy and only 277032 non-bouncy numbers below 10^10.
// How many numbers below a googol (10^100) are not bouncy?

[<TestFixture>]
type Problem113() =

    let calcIncreasedCountForFirstDigit (firstDigit: int) (prevIncreasedCount: int64[]) =
        seq {firstDigit .. 9} |> Seq.map (fun digit -> prevIncreasedCount.[digit]) |> Seq.sum

    let calcDecreasedCountForFirstDigit (lastDigit: int) (prevDecreasedCount: int64[]) =
        seq {0 .. lastDigit} |> Seq.map (fun digit -> prevDecreasedCount.[digit]) |> Seq.sum

    let rec processNumbersRange (digitsCount: int) (maxDigitsCount: int) (prevIncreasedCount: int64[]) (prevDecreasedCount: int64[]) (nonBouncyCount: int64) =
        match digitsCount with
        | _ when digitsCount > maxDigitsCount -> nonBouncyCount
        | _ ->
            let increasedCount = Array.create 10 0L
            let decreasedCount = Array.create 10 0L
            seq {1 .. 9} |> Seq.iter (fun digit -> increasedCount.[digit] <- (prevIncreasedCount |> calcIncreasedCountForFirstDigit digit))
            seq {0 .. 9} |> Seq.iter (fun digit -> decreasedCount.[digit] <- (prevDecreasedCount |> calcDecreasedCountForFirstDigit digit))
            // 1..1, 2..2, 3..3, 4..4, 5..5, 6..6, 7..7, 8..8, 9..9 are counted twice & 0 isn't positive number
            nonBouncyCount + (increasedCount |> Array.sum) + (decreasedCount |> Array.sum) - 9L - 1L |> processNumbersRange (digitsCount + 1) maxDigitsCount increasedCount decreasedCount

    let solveImpl (maxDigitsCount: int) =
        // 1 .. 9
        9L |> processNumbersRange 2 maxDigitsCount [|0L; 1L; 1L; 1L; 1L; 1L; 1L; 1L; 1L; 1L|] [|1L; 1L; 1L; 1L; 1L; 1L; 1L; 1L; 1L; 1L|]

    [<TestCase(6, 12951L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10, 277032L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 51161058134250L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxDigitsCount: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxDigitsCount)
