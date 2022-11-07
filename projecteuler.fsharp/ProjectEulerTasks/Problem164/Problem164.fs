namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem164() =

    [<Literal>]
    let StorageSize = 100

    let usedCombinations = seq {00 .. 99} |> Seq.filter (fun n -> (n / 10) + (n % 10) <= 9)

    let createInitData () =
        let initData = StorageSize |> Array.zeroCreate
        for number in usedCombinations |> Seq.filter (fun n -> n >= 10) do
            initData.[number] <- 1L
        initData

    let generateNextStepData (prevStepData: int64[]) =
        let nextStepData = StorageSize |> Array.zeroCreate
        for combination in usedCombinations do
            let firstDigit = combination / 10
            let secondDigit = combination % 10
            for thidDigit in 0 .. (9 - firstDigit - secondDigit) do
                nextStepData.[secondDigit * 10 + thidDigit] <- nextStepData.[secondDigit * 10 + thidDigit] + prevStepData.[combination]
        nextStepData

    // TODO (std_string) : add solution via combinatorics
    let solveImpl (size: int) =
        seq {1 .. size - 2} |> Seq.fold (fun prevStepData _ -> prevStepData |> generateNextStepData) (createInitData ()) |> Seq.sum

    [<TestCase(3, 165L, TimeThresholds.HardTimeLimit)>]
    [<TestCase(20, 378158756814587L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(size: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, size)