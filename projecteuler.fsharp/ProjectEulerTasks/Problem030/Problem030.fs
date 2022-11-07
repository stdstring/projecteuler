namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

[<TestFixture>]
type Problem030() =

    let rec findInfimum (value: int) (sum: int) (delta: int) =
        match value with
        | _ when value < sum -> findInfimum (value * 10) (sum + delta) delta
        | _ -> sum

    let solveImpl (power: int) =
        let delta = pown 9 power
        let infimum = findInfimum 1 delta delta
        seq {2 .. infimum} |> Seq.filter (fun number -> number |> NumbersDigits.GetDigits |> Seq.map (fun(digit) -> pown digit power) |> Seq.sum = number) |> Seq.sum

    [<TestCase(4, 19316, TimeThresholds.HardTimeLimit)>]
    [<TestCase(5, 443839, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(power: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, power)
