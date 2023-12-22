namespace ProjectEulerTasks

open CommonLib
open NUnit.Framework
open ProjectEulerTasks.Utils
open System.Collections.Generic

[<TestFixture>]
type Problem009() =

    let solveImpl (maxPerimeter: int) =
        let generator = PythagoreanTriplesGenerator(fun triple -> triple.X + triple.Y + triple.Z)
        let triples = generator.GenerateTriples(maxPerimeter)
        let storage = Dictionary<int, ResizeArray<PythagoreanTriple>>()
        for triple in triples do
            let perimeter = triple.X + triple.Y + triple.Z
            if not (storage.ContainsKey(perimeter)) then
                storage.[perimeter] <- ResizeArray<PythagoreanTriple>()
            storage.[perimeter].Add(triple)
        Assert.That(storage.ContainsKey(maxPerimeter), Is.True);
        Assert.That(storage.[maxPerimeter].Count, Is.EqualTo(1));
        let actualTriple = storage.[maxPerimeter].[0]
        actualTriple.X * actualTriple.Y * actualTriple.Z

    [<TestCase(1000, 31875000, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxPerimeter: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxPerimeter)