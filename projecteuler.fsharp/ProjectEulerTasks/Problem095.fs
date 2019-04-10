namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

//  The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14.
// As the sum of these divisors is equal to 28, we call it a perfect number.
// Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers.
// For this reason, 220 and 284 are called an amicable pair. Perhaps less well known are longer chains.
// For example, starting with 12496, we form a chain of five numbers: 12496 -> 14288 -> 15472 -> 14536 -> 14264 -> 12496 -> ...
// Since this chain returns to its starting point, it is called an amicable chain.
// Find the smallest member of the longest amicable chain with no element exceeding one million.

module Problem095Impl =
    type ChainData = {Chain: int list; ChainSet: ISet<int>}

open Problem095Impl

[<TestFixture>]
type Problem095() =

    [<Literal>]
    let MinNumber = 1

    let rec processChain (dividerStorage: NumbersDividersStorage) (chainStorage: int option[]) (currentNumber: int) (maxNumber: int) (chain: ChainData) =
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
                let nextNumber = (currentNumber |> dividerStorage.GetDividersSet |> Seq.sum) - currentNumber
                currentNumber |> chain.ChainSet.Add |> ignore
                {chain with Chain = currentNumber :: chain.Chain} |> processChain dividerStorage chainStorage nextNumber maxNumber

    let solveImpl (maxNumber: int) =
        let dividerStorage = maxNumber |> NumbersDividersStorageFactory.CreateDividersStorage
        let chainStorage = Array.create (maxNumber - MinNumber) None
        chainStorage.[0]<-(0 |> Some)
        seq {MinNumber + 1 .. maxNumber - 1} |> Seq.iter (fun number -> {ChainData.Chain = []; ChainData.ChainSet = new HashSet<int>()} |> processChain dividerStorage chainStorage number maxNumber)
        let mutable bestLength, bestNumber = chainStorage.[0].Value, MinNumber
        for number in seq {MinNumber + 1 .. maxNumber - 1} do
            if (bestLength < chainStorage.[number - MinNumber].Value) then
                bestLength<-chainStorage.[number - MinNumber].Value
                bestNumber<-number
        bestNumber |> int

    [<TestCase(1000000, 14316, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)
