namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open System
open System.IO

[<TestFixture>]
type Problem008() =

    let solveImpl (dataFilename: string) (count: int) =
        let content = File.ReadAllText(Path.Combine("Data", dataFilename))
        let digits = content |> Seq.map (fun ch -> Char.GetNumericValue(ch) |> int) |> Seq.toArray
        let foldFun maxProduct start =
            let product = seq {0 .. count - 1} |> Seq.fold (fun value index -> value * digits.[start + index]) 1
            match product with
            | _ when product > maxProduct -> product
            | _ -> maxProduct
        seq {0 .. digits.Length - count} |> Seq.fold foldFun 0

    [<TestCase("problem_008.dat", 4, 5832, TimeThresholds.HardTimeLimit)>]
    [<TestCase("problem_008.dat", 5, 40824, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dataFilename: string, count: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dataFilename, count)