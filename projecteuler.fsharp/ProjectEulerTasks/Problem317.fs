namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// A firecracker explodes at a height of 100 m above level ground. It breaks into a large number of very small fragments,
// which move in every direction; all of them have the same initial velocity of 20 m/s.
// We assume that the fragments move without air resistance, in a uniform gravitational field with g=9.81 m/s^2.
// Find the volume (in m^3) of the region through which the fragments move before reaching the ground. Give your answer rounded to four decimal places.

[<TestFixture>]
type Problem317() =

    [<Literal>]
    let g = 9.81

    let solveImpl (height: double) (v0: double) =
        // Parabola of safety has the following equation: y = height + 1 / (4 * a) - a * x^2, where a = g / (2 * v0^2)
        // y = 0 when xMax = sqrt((height + 1 / (4 * a)) / a)
        let a = g / (2.0 * v0 * v0)
        let xMax = (height + 1.0 / (4.0 * a)) / a |> sqrt
        // volume of a body of revolution
        let areaVolume = System.Math.PI * (height * (pown xMax 2) + (pown xMax 2) / (4.0 * a) - a * (pown xMax 4) / 2.0)
        System.Math.Round(areaVolume, 4)

    [<TestCase(100.0, 20.0, 1856532.8455, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(height: double, v0: double, expectedAnswer: double, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, height, v0)
