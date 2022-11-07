namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

module Problem012Impl =
    type TriangleData = {Prev: int; N: int}

open Problem012Impl

[<TestFixture>]
type Problem012() =

    let solveImpl (dividersCountInfimum: int) =
        let generator (data: TriangleData) =
            let nextN = data.N + 1
            let nextValue = data.Prev + nextN
            Some (nextValue, {TriangleData.Prev = nextValue; TriangleData.N = nextN})
        {TriangleData.Prev = 0; TriangleData.N = 0} |> Seq.unfold generator |> Seq.skipWhile (fun value -> NumbersDividers.GetDividers(value).Length < dividersCountInfimum) |> Seq.head


    [<TestCase(6, 28, TimeThresholds.HardTimeLimit)>]
    [<TestCase(501, 76576500, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(dividersCountInfimum: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, dividersCountInfimum)