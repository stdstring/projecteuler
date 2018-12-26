namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// The Fibonacci sequence is defined by the recurrence relation:
// F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.
// Hence the first 12 terms will be:
// F(1) = 1
// F(2) = 1
// F(3) = 2
// F(4) = 3
// F(5) = 5
// F(6) = 8
// F(7) = 13
// F(8) = 21
// F(9) = 34
// F(10) = 55
// F(11) = 89
// F(12) = 144
// The 12th term, F(12), is the first term to contain three digits.
// What is the first term in the Fibonacci sequence to contain 1000 digits?

module Problem025Impl =
    type FibonacciData = {Prev: bigint; Current: bigint; TermNumber: int}

open Problem025Impl

[<TestFixture>]
type Problem025() =

    let solveImpl (digitsCount: int) =
        let infimumValue = bigint.Pow(10I, digitsCount - 1)
        let generator (data: FibonacciData) =
            let nextValue = data.Prev + data.Current
            let nextTermNumber = data.TermNumber + 1
            Some ((nextValue, nextTermNumber), {FibonacciData.Prev = data.Current; FibonacciData.Current = nextValue; FibonacciData.TermNumber = nextTermNumber})
        let _, termNumber = {FibonacciData.Prev = 1I; FibonacciData.Current = 0I; FibonacciData.TermNumber = 0} |>
                            Seq.unfold generator |>
                            Seq.skipWhile (fun (value, termNumber) -> value < infimumValue) |>
                            Seq.head
        termNumber

    [<TestCase(3, 12, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 4782, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(digitsCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, fun() -> solveImpl digitsCount)