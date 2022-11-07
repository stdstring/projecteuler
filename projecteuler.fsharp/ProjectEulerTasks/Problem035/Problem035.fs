namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

[<TestFixture>]
type Problem035() =

    let processNumber (primeNumbersSet: ISet<int>) (knownPrimes: ISet<int>) (prime: int) =
        match knownPrimes.Contains(prime) with
        | true -> ()
        | false ->
            let allCircularNumbers = prime |> NumbersDigits.GetDigits |> ListUtils.GetAllCirularShift |> List.map (fun digits -> digits |> NumbersDigits.GetNumber |> int)
            match allCircularNumbers |> List.exists (fun number -> number |> primeNumbersSet.Contains |> not) with
            | true -> ()
            | false -> allCircularNumbers |> List.iter (fun number -> number |> knownPrimes.Add |> ignore)

    let solveImpl (maxNumber: int) =
        let primeNumbers = EratosSieve.Create(maxNumber).ToSeq() |> Seq.toList
        let primeNumbersSet = HashSet<int>(primeNumbers) :> ISet<int>
        let knownPrimes = HashSet<int>() :> ISet<int>
        primeNumbers |> List.iter (fun prime -> prime |> processNumber primeNumbersSet knownPrimes)
        knownPrimes.Count

    [<TestCase(99, 13, TimeThresholds.HardTimeLimit)>]
    [<TestCase(999999, 55, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
