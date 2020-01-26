namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// Euler's Totient function, phi(n) [sometimes called the phi function], is used to determine the number of numbers less than n which are relatively prime to n.
// For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, phi(9)=6.
// n = 2,  Relatively Prime = 1,             phi(n) = 1, n/phi(n) = 2
// n = 3,  Relatively Prime = {1,2},         phi(n) = 2, n/phi(n) = 1.5
// n = 4,  Relatively Prime = {1,3},         phi(n) = 2, n/phi(n) = 2
// n = 5,  Relatively Prime = {1,2,3,4},     phi(n) = 4, n/phi(n) = 1.25
// n = 6,  Relatively Prime = {1,5},         phi(n) = 2, n/phi(n) = 3
// n = 7,  Relatively Prime = {1,2,3,4,5,6}, phi(n) = 6, n/phi(n) = 1.1666...
// n = 8,  Relatively Prime = {1,3,5,7},     phi(n) = 4, n/phi(n) = 2
// n = 9,  Relatively Prime = {1,2,4,5,7,8}, phi(n) = 6, n/phi(n) = 1.5
// n = 10, Relatively Prime = {1,3,7,9},     phi(n) = 4, n/phi(n) = 2.5
// It can be seen that n=6 produces a maximum n/phi(n) for n <= 10.
// Find the value of n <= 1,000,000 for which n/phi(n) is a maximum.

[<TestFixture>]
type Problem069() =

    let solveImpl (maxNumber: int) =
        let eulerTotientFunction = maxNumber |> EulerTotientFunction.Create
        seq {2 .. maxNumber} |> Seq.maxBy (fun number -> (number |> float) / (number |> eulerTotientFunction.GetValue |> float))

    [<TestCase(10, 6, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 510510, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
