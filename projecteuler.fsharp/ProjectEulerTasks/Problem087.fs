namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28.
// In fact, there are exactly four numbers below fifty that can be expressed in such a way:
// 28 = 2^2 + 2^3 + 2^4
// 33 = 3^2 + 2^3 + 2^4
// 49 = 5^2 + 2^3 + 2^4
// 47 = 2^2 + 3^3 + 2^4
// How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?

[<TestFixture>]
type Problem087() =

    let solveImpl (numberSup: int) =
        let primeSup = numberSup |> float |> sqrt |> int |> (+) 1
        let sieveBuilder = EratosSieveBuilder()
        let primes = sieveBuilder.CreateSieve(primeSup).ToSeq() |> Seq.toList
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