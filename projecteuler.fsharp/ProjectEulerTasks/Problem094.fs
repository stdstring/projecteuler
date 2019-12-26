namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// It is easily proved that no equilateral triangle exists with integral length sides and integral area.
// However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
// We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.
// Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

[<TestFixture>]
type Problem094() =

    let d = 3

    let solveImpl (maxPerimeter: int) =
        // we have the triangle with sides C, C, C+-1
        // S^2 = (p/2)*((p/2) - C)*((p/2) - C)*((p/2) - (C+-1)) = p*(p - 2*C)*(p - 2*C)*(p - 2*(C+-1))/16 where p = C + C + (C+-1) - perimeter
        // if C - even => C+-1 - odd => p - odd, 2*(C+-1) - even => p - 2*(C+-1) - odd, p - 2*C -odd =>
        // p*(p - 2*C)*(p - 2*C)*(p - 2*(C+-1)) - odd => p*(p - 2*C)*(p - 2*C)*(p - 2*(C+-1)) isn't divided by 16 => C - odd, C+-1 - even
        // Let B - height (and median) on side C+-1 => 2*A = C+-1 and triangle with sides A, B, C is right triangle
        // 2*A = C+-1 or C = 2*A+-1 - equivalent definitions
        // A^2 + B^2 = C^2 = (2*A+-1)^2 = 4*A^2 +- 4*A + 1 =>
        // B^2 = 4*A^2 +- 4*A + 1 - A^2 = 3*A^2 +- 4*A + 1 => 3*B^2 = 9*A^2 +- 12*A + 3 = (9*A^2 +- 12*A + 4) - 1 = (3*A+-2)^2 - 1
        // so, 3*B^2 = (3*A+-2)^2 - 1 => (3*A+-2)^2 - 3*B^2 = 1 - Pell equation
        let firstSolution = PellEquation.FindFirstSolution(d, 1).Value
        let calcPerimeter (x: bigint) (sign: bigint) =
            // Not all values of x are leads to the integer values of sides
            let value = x - 2I * sign
            match value with
            | _ when (value = 0I) || (value % 3I <> 0I) -> 0I
            | _ ->
                let a = value / 3I
                let c = 2I * a + sign
                2I * (c + a)
        let rec calcPerimeterSum (solution: PellSolution) (n: int) (result: bigint) =
            let maxPerimeter = maxPerimeter |> bigint
            // X = 3*A+-2
            // X1 = 3*A + 2
            // A1 = (X1 - 2) / 3
            // C1 = 2 * A1 + 1
            // P1 = 2 * C1 + 2 * A1
            let p1 = calcPerimeter solution.X 1I
            // X1 = 3*A + 2
            // A2 = (X2 + 2) / 3
            // C2 = 2 * A1 - 1
            // P2 = 2 * C2 + 2 * A2
            let p2 = calcPerimeter solution.X -1I
            match p1, p2 with
            | _ when (p1 > maxPerimeter) && (p2 > maxPerimeter) -> result
            | _ when (p1 <= maxPerimeter) && (p2 > maxPerimeter) -> result + p1
            | _ when (p1 > maxPerimeter) && (p2 <= maxPerimeter) -> result + p2
            | _ -> calcPerimeterSum (PellEquation.FindNSolution(firstSolution, d, 1, n + 1)) (n + 1) (result + p1 + p2)
        calcPerimeterSum firstSolution 1 0I |> int

    [<TestCase(1000000000, 518408346, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxPerimeter: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxPerimeter)