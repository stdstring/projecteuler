namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

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
                            Seq.skipWhile (fun (value, _) -> value < infimumValue) |>
                            Seq.head
        termNumber

    [<TestCase(3, 12, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 4782, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(digitsCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, digitsCount)
