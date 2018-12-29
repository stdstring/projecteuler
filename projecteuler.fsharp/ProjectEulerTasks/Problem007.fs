namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13. What is the 10001st prime number?

[<TestFixture>]
type Problem007() =

    let checkIsPrime (number: int) (primes: ResizeArray<int>) =
        let checkUpperBound = (number |> float |> sqrt |> int) + 1
        primes |> Seq.takeWhile (fun prime -> prime <= checkUpperBound) |> Seq.exists (fun prime -> number % prime = 0) |> not

    let rec traverseNumber (number: int) (desiredCount: int) (primes: ResizeArray<int>) =
        match primes with
        | _ when primes.Count = desiredCount -> primes
        | _ ->
            if (primes |> checkIsPrime number) then primes.Add(number) else ()
            primes |> traverseNumber (number + 2) desiredCount

    let solveImpl (desiredCount: int) =
        let primes = ResizeArray<int>(desiredCount)
        primes.Add(2)
        primes |> traverseNumber 3 desiredCount |> ignore
        primes.[desiredCount - 1]

    [<TestCase(6, 13, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10001, 104743, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(desiredCount: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, desiredCount)
