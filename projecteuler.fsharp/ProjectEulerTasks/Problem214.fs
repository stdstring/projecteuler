namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// Let phi be Euler's totient function, i.e. for a natural number n, phi(n) is the number of k, 1 <= k <= n, for which gcd(k,n) = 1.
// By iterating phi, each positive integer generates a decreasing chain of numbers ending in 1.
// E.g. if we start with 5 the sequence 5,4,2,1 is generated.
// Here is a listing of all chains with length 4:
// 5,4,2,1
// 7,6,2,1
// 8,4,2,1
// 9,6,2,1
// 10,4,2,1
// 12,4,2,1
// 14,6,2,1
// 18,6,2,1
// Only two of these chains start with a prime, their sum is 12.
// What is the sum of all primes less than 40000000 which generate a chain of length 25?

[<TestFixture>]
type Problem214() =

    let rec processChain (eulerTotientFunction: EulerTotientFunction) (length: int) (number: int) =
        match number with
        | 1 -> length + 1
        | _ -> number |> eulerTotientFunction.GetValue |> processChain eulerTotientFunction (length + 1)

    let solveImpl (maxNumber: int) (chainLength: int) =
        let eulerTotientFunction = maxNumber |> EulerTotientFunction.Create
        let mutable result = 0L
        for prime in seq {3 .. 2 .. maxNumber} |> Seq.filter (fun number -> number |> eulerTotientFunction.IsPrime) do
            if (prime |> processChain eulerTotientFunction 0) = chainLength then
                result<-result + (prime |> int64)
        result |> int64

    [<TestCase(20, 4, 12L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(40000000, 25, 1677366278943L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, chainLength: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, chainLength)
