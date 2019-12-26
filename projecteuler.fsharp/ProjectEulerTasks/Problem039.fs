namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120: {20, 48, 52}, {24, 45, 51}, {30, 40, 50}
// For which value of p <= 1000, is the number of solutions maximised?

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
