namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

// Euler's Totient function, phi(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n.
// For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, phi(9)=6.
// The number 1 is considered to be relatively prime to every positive number, so phi(1)=1.
// Interestingly, phi(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
// Find the value of n, 1 < n < 10^7, for which phi(n) is a permutation of n and the ratio n/phi(n) produces a minimum.

[<TestFixture>]
type Problem070() =

    let isPermutation (number1: int) (number2: int) =
        let foldFun (storage: int[]) (digit: int) =
            storage.[digit]<-storage.[digit] + 1
            storage
        let number1Representation = number1 |> NumbersDigits.GetDigits |> List.fold foldFun (Array.create 10 0)
        let number2Representation = number2 |> NumbersDigits.GetDigits |> List.fold foldFun (Array.create 10 0)
        number1Representation = number2Representation

    let solveImpl (maxNumber: int) =
        let eulerTotientFunction = maxNumber |> EulerTotientFunction.Create
        let mutable bestNumber = 2
        // for n = 2 phi(n) = 1 => n / phi(n) = 2
        let mutable bestRatio = 2.0
        for number in {2 .. maxNumber} do
            let phiValue = number |> eulerTotientFunction.GetValue
            let ratio = (number |> float) / (phiValue |> float)
            if (ratio < bestRatio) && (number / phiValue < 10) && (isPermutation number phiValue) then
                bestNumber<-number
                bestRatio<-ratio
        bestNumber

    [<TestCase(10000000, 8319823, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
