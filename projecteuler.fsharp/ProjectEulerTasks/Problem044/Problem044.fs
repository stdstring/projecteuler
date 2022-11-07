namespace ProjectEulerTasks

open Checked
open System
open NUnit.Framework
open ProjectEulerTasks.Utils

// TODO (std_string) : think about solve via solution of the corresponding Pell/Diophantine equation
[<TestFixture>]
type Problem044() =

    // TODO (std_string) : think about choosing this value
    [<Literal>]
    let MaxNumber = 10000

    let calcPentagonalNumber (n: int) = n * (3 * n - 1) / 2

    let createPentagonalNumberStorage () =
        let storage = Array.create (MaxNumber + 1) 0
        seq {1 .. MaxNumber} |> Seq.iter (fun n -> storage.[n] <- n |> calcPentagonalNumber)
        storage

    let createPentagonalNumberCheckStorage (pentagonalNumberStorage: int[]) =
        let storage = Array.create ((pentagonalNumberStorage |> Array.last) + 1) false
        seq {1 .. MaxNumber} |> Seq.iter (fun n -> storage.[pentagonalNumberStorage.[n]] <- true)
        storage

    let solveImpl () =
        let pentagonalNumberStorage = createPentagonalNumberStorage ()
        let pentagonalNumberCheckStorage = pentagonalNumberStorage |> createPentagonalNumberCheckStorage
        let mutable minDifference = Int32.MaxValue
        for j in {1 .. MaxNumber} do
            for k in {j .. MaxNumber} do
                let difference = pentagonalNumberStorage.[k] - pentagonalNumberStorage.[j]
                let sum = pentagonalNumberStorage.[k] + pentagonalNumberStorage.[j]
                // TODO (std_string) : think about such approach
                if (pentagonalNumberCheckStorage.[difference]) && (sum < pentagonalNumberCheckStorage.Length) && (pentagonalNumberCheckStorage.[sum]) then
                    minDifference <- min minDifference difference
        minDifference

    [<TestCase(5482660, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl)
