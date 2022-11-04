namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

[<TestFixture>]
type Problem075() =

    let solveImpl (maxPerimeter: int) =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X + triple.Y + triple.Z)
        let triples = generator.GenerateTriples(maxPerimeter)
        let storage = Dictionary<int, int>()
        for triple in triples do
            let perimeter = triple.X + triple.Y + triple.Z
            if not (storage.ContainsKey(perimeter)) then
                storage.[perimeter] <- 0
            storage.[perimeter] <- storage.[perimeter] + 1
        storage |> Seq.filter (fun kvPair -> kvPair.Value = 1) |> Seq.length

    [<TestCase(1500000, 161667, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxPerimeter: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxPerimeter)
