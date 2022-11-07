namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem124() =

    [<Literal>]
    let MinValue = 1

    let calcRadical (sieve: EratosSieveWithSmallestPrimeFactors) (number: int) =
        let rec eraseFactor (factor: int) (number: int) =
            match number % factor with
            | 0 -> (number / factor) |> eraseFactor factor
            | _ -> number
        let mutable rest = number
        let mutable radical = 1
        if number % 2 = 0 then
            rest <- number |> eraseFactor 2
            radical <- 2 * radical
        while (rest > 1) do
            let factor = sieve.[rest]
            radical <- radical * factor
            rest <- rest |> eraseFactor factor
        radical

    let findValueByPosition (dest: int list []) (position: int) =
        let rec findValueByPositionImpl (index: int) (currentPosition: int) =
            match currentPosition + dest.[index].Length with
            | nextPosition when (currentPosition <= position) && (nextPosition > position) ->
                dest.[index] |> List.sort |> List.skip (position - currentPosition) |> List.head
            | nextPosition -> nextPosition |> findValueByPositionImpl (index + 1)
        1 |> findValueByPositionImpl 0

    let solveImpl (maxNumber: int) (position: int) =
        let sieve = maxNumber |> EratosSieveWithSmallestPrimeFactors.Create
        // maxValue <= maxNumber obviously
        let dest = Array.create maxNumber []
        dest.[0]<-[1]
        let iterFun (number: int) =
            let radicalValue = number |> calcRadical sieve
            dest.[radicalValue - MinValue]<-(number :: dest.[radicalValue - MinValue])
        // maxValue <= maxNumber obviously
        seq {MinValue + 1 .. maxNumber} |> Seq.iter iterFun
        position |> findValueByPosition dest

    [<TestCase(10, 4, 8, TimeThresholds.HardTimeLimit)>]
    [<TestCase(10, 6, 9, TimeThresholds.HardTimeLimit)>]
    [<TestCase(100000, 10000, 21417, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxNumber: int, position: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxNumber, position)
