﻿namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
// (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
// There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
// What 12-digit number do you form by concatenating the three terms in this sequence?

[<TestFixture>]
type Problem049() =

    let minNumber = 1001
    let maxNumber = 9999
    let knownResult = 148748178147L

    let generatePossibleTerms (number: int) =
        let [d1; d2; d3; d4] = number |> NumbersDigits.GetDigits
        [[d1; d2; d4; d3]; [d1; d3; d2; d4]; [d1; d3; d4; d2]; [d1; d4; d2; d3]; [d1; d4; d3; d2];
         [d2; d1; d3; d4]; [d2; d1; d4; d3]; [d2; d3; d1; d4]; [d2; d3; d4; d1]; [d2; d4; d1; d3]; [d2; d4; d3; d1];
         [d3; d1; d2; d4]; [d3; d1; d4; d2]; [d3; d2; d1; d4]; [d3; d2; d4; d1]; [d3; d4; d1; d2]; [d3; d4; d2; d1];
         [d4; d1; d2; d3]; [d4; d1; d3; d2]; [d4; d2; d1; d3]; [d4; d2; d3; d1]; [d4; d3; d1; d2]; [d4; d3; d2; d1]] |> List.map (fun digits -> digits |> NumbersDigits.GetNumber |> int)

    let chooseTerms (primesSet: ISet<int>) (prime: int) =
        let possibleTerms = prime |> generatePossibleTerms
        let possibleTermsSet = possibleTerms |> HashSet<int> :> ISet<int>
        let chooseTerms (secondTerm: int) =
            let delta = secondTerm - prime
            let thirdTerm = secondTerm + delta
            match secondTerm with
            | _ when primesSet.Contains(secondTerm) && (delta > 0) && primesSet.Contains(thirdTerm) && possibleTermsSet.Contains(thirdTerm) ->
                Some ((prime |> int64) * 100000000L + (secondTerm |> int64) * 10000L + (thirdTerm |> int64))
            | _ -> None
        possibleTerms |> List.filter (fun number -> number >= prime) |> List.choose chooseTerms

    let solveImpl () =
        let primes = EratosSieve.Create(maxNumber).ToSeq() |> Seq.skipWhile (fun prime -> prime < minNumber) |> Seq.toList
        let primesSet = primes |> HashSet<int> :> ISet<int>
        let answers = primes |> Seq.map (fun prime -> prime |> chooseTerms primesSet) |> Seq.concat |> Seq.filter (fun answer -> answer <> knownResult) |> Seq.distinct |> Seq.toArray
        Assert.AreEqual(1, answers.Length)
        answers.[0]

    [<TestCase(296962999629L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)