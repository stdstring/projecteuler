namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib
open System.Collections.Generic

// A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2
// For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
// There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product a * b * c.

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
        Assert.IsTrue(storage.ContainsKey(maxPerimeter))
        Assert.AreEqual(1, storage.[maxPerimeter].Count)
        let actualTriple = storage.[maxPerimeter].[0]
        actualTriple.X * actualTriple.Y * actualTriple.Z

    [<TestCase(1000, 31875000, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxPerimeter: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxPerimeter)