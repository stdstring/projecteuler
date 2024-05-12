namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

[<TestFixture>]
type Problem049() =

    let minNumber = 1001
    let maxNumber = 9999
    let knownResult = 148748178147L

    let generatePossibleTerms (number: int) =
        match number |> NumbersDigits.GetDigits with
        | [d1; d2; d3; d4] ->
            [[d1; d2; d4; d3];
             [d1; d3; d2; d4];
             [d1; d3; d4; d2];
             [d1; d4; d2; d3];
             [d1; d4; d3; d2];
             [d2; d1; d3; d4];
             [d2; d1; d4; d3];
             [d2; d3; d1; d4];
             [d2; d3; d4; d1];
             [d2; d4; d1; d3];
             [d2; d4; d3; d1];
             [d3; d1; d2; d4];
             [d3; d1; d4; d2];
             [d3; d2; d1; d4];
             [d3; d2; d4; d1];
             [d3; d4; d1; d2];
             [d3; d4; d2; d1];
             [d4; d1; d2; d3];
             [d4; d1; d3; d2];
             [d4; d2; d1; d3];
             [d4; d2; d3; d1];
             [d4; d3; d1; d2];
             [d4; d3; d2; d1]] |> List.map (fun digits -> digits |> NumbersDigits.GetNumber |> int)
        | _ -> failwith "Unexpected branch of match expression"

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
        Assert.That(answers.Length, Is.EqualTo(1))
        answers.[0]

    [<TestCase(296962999629L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)