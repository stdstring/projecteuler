namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

module Problem050Impl =
    type PrimesSumData = {Sum: int; Count: int}

open Problem050Impl

[<TestFixture>]
type Problem050() =

    let rec processSum (maxNumber: int) (sieve: EratosSieve) (primes: int list) (bestData: PrimesSumData) (currentData: PrimesSumData) =
        match primes with
        | [] -> bestData
        | prime :: _ when prime + currentData.Sum >= maxNumber -> bestData
        | prime :: primesRest ->
            let newSum = currentData.Sum + prime
            let newCount = currentData.Count + 1
            match newSum |> sieve.IsPrime with
            | false -> {PrimesSumData.Sum = newSum; PrimesSumData.Count = newCount} |> processSum maxNumber sieve primesRest bestData
            | true when newCount <= bestData.Count -> {PrimesSumData.Sum = newSum; PrimesSumData.Count = newCount} |> processSum maxNumber sieve primesRest bestData
            | true ->
                let newBestData = {PrimesSumData.Sum = newSum; PrimesSumData.Count = newCount}
                processSum maxNumber sieve primesRest newBestData newBestData

    let rec processPrimes (maxNumber: int) (sieve: EratosSieve) (primes: int list) (bestData: PrimesSumData) =
        match primes with
        | [] -> bestData
        | prime :: primesRest -> {PrimesSumData.Sum = prime; PrimesSumData.Count = 1} |> processSum maxNumber sieve primesRest bestData |> processPrimes maxNumber sieve primesRest

    let solveImpl (maxNumber: int) =
        let sieve = maxNumber |> EratosSieve.Create
        let primes = sieve.ToSeq() |> Seq.toList
        let result = {PrimesSumData.Sum = 2; PrimesSumData.Count = 1} |> processPrimes maxNumber sieve primes
        result.Sum

    [<TestCase(100, 41, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000, 953, TimeThresholds.HardTimeLimit)>]
    [<TestCase(1000000, 997651, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber)