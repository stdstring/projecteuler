namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

module Problem095Impl =
    type ChainData = {Chain: int list; ChainSet: ISet<int>}

open Problem095Impl

[<TestFixture>]
type Problem095() =

    [<Literal>]
    let MinNumber = 1

    let rec processChain (sieve: EratosSieveWithSmallestPrimeFactors) (chainStorage: int option[]) (currentNumber: int) (maxNumber: int) (chain: ChainData) =
        match currentNumber with
        | _ when currentNumber >= maxNumber ->
            chain.Chain |> Seq.iter (fun number -> chainStorage.[number - MinNumber]<-(0 |> Some))
        | _ ->
            let currentValue = chainStorage.[currentNumber - MinNumber]
            match currentNumber, currentValue with
            | _, Some _ ->
                chain.Chain |> Seq.iteri (fun index number -> chainStorage.[number - MinNumber]<-(0 |> Some))
            | _, None when currentNumber |> chain.ChainSet.Contains ->
                let cycleLength = chain.Chain |> Seq.takeWhile (fun number -> number <> currentNumber)|> Seq.length |> (+) 1
                let iterFun (index: int) (number: int) =
                    match index with
                    | _ when index < cycleLength -> chainStorage.[number - MinNumber]<-(cycleLength |> Some)
                    | _ -> chainStorage.[number - MinNumber]<-(0 |> Some)
                chain.Chain |> Seq.iteri iterFun
            | _, None ->
                let nextNumber = (currentNumber |> sieve.CalcSigma1) - currentNumber
                currentNumber |> chain.ChainSet.Add |> ignore
                {chain with Chain = currentNumber :: chain.Chain} |> processChain sieve chainStorage nextNumber maxNumber

    let solveImpl (maxNumber: int) =
        let spf = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        let chainStorage = Array.create (maxNumber - MinNumber) None
        chainStorage.[0]<-(0 |> Some)
        seq {MinNumber + 1 .. maxNumber - 1} |> Seq.iter (fun number -> {ChainData.Chain = []; ChainData.ChainSet = new HashSet<int>()} |> processChain spf chainStorage number maxNumber)
        let mutable bestLength, bestNumber = chainStorage.[0].Value, MinNumber
        for number in seq {MinNumber + 1 .. maxNumber - 1} do
            if (bestLength < chainStorage.[number - MinNumber].Value) then
                bestLength<-chainStorage.[number - MinNumber].Value
                bestNumber<-number
        bestNumber |> int

    [<TestCase(1000000, 14316, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
