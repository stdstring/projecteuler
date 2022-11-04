namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

[<TestFixture>]
type Problem029() =

    let minBaseValue = 2
    let minExpValue = 2

    let findAvailablePowers (value: int) (maxBaseValue: int) (maxExpValue: int) =
        seq {1 .. maxExpValue} |> Seq.takeWhile (fun expValue -> maxBaseValue >= pown value expValue) |> Seq.toList

    let rec findAllAvailablePowers (value: int) (maxBaseValue: int) (maxExpValue: int) (knownPowers: ISet<int>) (count: int) (dest: int list) =
         match value with
         | _ when value * value > maxBaseValue -> count, dest
         | _ when knownPowers.Contains(value) -> findAllAvailablePowers (value + 1) maxBaseValue maxExpValue knownPowers count dest
         | _ ->
            let powers = findAvailablePowers value maxBaseValue maxExpValue
            powers |> List.iter (fun power -> pown value power |> knownPowers.Add |> ignore)
            let powersSize = powers |> List.length
            findAllAvailablePowers (value + 1) maxBaseValue maxExpValue knownPowers (powersSize + count) (powersSize :: dest)

    let calcPowersDistintCount (maxExpValue: int) (powersSize: int) =
        let distinctValuesStorage = new HashSet<int>() :> ISet<int>
        seq {1 .. powersSize} |> Seq.iter (fun factor -> seq {minExpValue .. maxExpValue} |> Seq.iter (fun powerValue -> factor * powerValue |> distinctValuesStorage.Add |> ignore))
        distinctValuesStorage.Count

    let solveImpl (maxBaseValue: int) (maxExpValue: int) =
        let powersTotalCount, powersSizes = findAllAvailablePowers minBaseValue maxBaseValue maxExpValue (new HashSet<int>() :> ISet<int>) 0 []
        let countBaseExpWithoutRepeat = (maxBaseValue - minBaseValue + 1 - powersTotalCount) * (maxExpValue - minExpValue + 1)
        let countBaseExpWithRepeat = powersSizes |> Seq.map (fun expSize -> expSize |> calcPowersDistintCount maxExpValue) |> Seq.sum
        countBaseExpWithoutRepeat + countBaseExpWithRepeat

    [<TestCase(5, 5, 15, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10, 10, 69, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100, 100, 9183, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxBaseValue: int, maxExpValue: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxBaseValue, maxExpValue)