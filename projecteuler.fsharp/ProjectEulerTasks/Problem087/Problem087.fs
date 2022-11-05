namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem087() =

    let solveImpl (numberSup: int) =
        let primeSup = numberSup |> float |> sqrt |> int |> (+) 1
        let primes = EratosSieve.Create(primeSup).ToSeq() |> Seq.toList
        let storage = Array.create numberSup false
        let processFourthPower (sum: int) =
            primes |> Seq.map (fun number -> pown number 4) |> Seq.takeWhile (fun number -> number <= (numberSup - sum)) |> Seq.iter (fun number -> storage.[sum + number - 1]<-true)
        let processCube (sum: int) =
            primes |> Seq.map (fun number -> pown number 3) |> Seq.takeWhile (fun number -> number <= (numberSup - sum)) |> Seq.iter (fun number -> sum + number |> processFourthPower)
        primes |> Seq.map (fun number -> number * number) |> Seq.iter (fun number -> number |> processCube)
        storage |> Seq.filter (fun value -> value) |> Seq.length

    [<TestCase(50, 4, TimeThresholds.HardTimeLimit)>]
    [<TestCase(50000000, 1097343, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(numberSup: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, numberSup)