namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

[<TestFixture>]
type Problem039() =

    let solveImpl (maxPerimeter: int) =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X + triple.Y + triple.Z)
        let triples = generator.GenerateTriples(maxPerimeter)
        let storage = Dictionary<int, int>()
        for triple in triples do
            let perimeter = triple.X + triple.Y + triple.Z
            if not (storage.ContainsKey(perimeter)) then
                storage.[perimeter] <- 0
            storage.[perimeter] <- storage.[perimeter] + 1
        (storage |> Seq.maxBy (fun kvPair -> kvPair.Value)).Key

    [<TestCase(1000, 840, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxPerimeter: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxPerimeter)
