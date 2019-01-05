namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin, O(0,0), to form OPQ triangle.
// There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies between 0 and 2 inclusive;
// that is, 0 <= x1, y1, x2, y2 <= 2 (see https://projecteuler.net/problem=91).
// Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can be formed?

[<TestFixture>]
type Problem091() =

    let solveImpl (maxX: int) (maxY: int) =
        // Math:
        // Let 0 <= x1, x2 <= MaxX, 0 <= y1, y2 <= MaxY
        // Let C1 - count of right triangles with legs, which are parallel to OX and OY axes
        // Let C2 - count of right triangles with legs, which are not parallel to OX and OY axes
        // Then total count of triangles = C1 + C2
        // Obviously, that C1 = 3 * MaxX * MaxY
        // Let (A, B) - arbitrary point with integer coordinates and 0 <= A <= MaxX, 0 <= B <= MaxY; R = (A, B) - radius vector of this point
        // Let N - vector perpendicular to R, then N = (-B, A) (or N = (B, -A))
        // Then equation of the line with the directing vector N and passing through the point (A, B) will be the following:
        // x = A - B * t / L, y = B + A * t / L, where t - parameter, L = sqrt(A^2 + B^2) - length of the vector N
        // t = - (x - A) * L / B => y = B - A * (X - A) / B
        // if y - integer, then A * (X - A) rem B = 0 => A * (X - A) = k * B, where k - integer (k = ..., -2, -1, 0, 1, 2, ...)
        // Let gcd(A, B) = N => B / A = B' / A' => x = A + k * B' / A' => k = ..., -2 * A', -A', 0, A', 2 * A', ...
        // y = B - A * (X - A) / B = B - A' * (X - A) / B' = B - A' * k * B' / (B' * A') = B - k
        // So, we can build the right triangles with integer coordinates with the following vertices:
        // 1) (0, 0), 2) (A, B), 3) (A + k * B' / A', B - k) k = ..., -2 * A', -A', 0, A', 2 * A', ...
        // Doing so for all possible values of A, B, k, we can calculate C2

        let calcC2CaseCount (a: int) (b: int) =
            let gcdValue = NumbersRelation.CalcGCD(a, b)
            let simpleA = a / gcdValue
            let simpleB = b / gcdValue
            (min ((maxX - a) / simpleB) (b / simpleA)) + (min (a / simpleB) ((maxY - b) / simpleA))
        let rec calcC2Count (x: int) (y: int) (count: int) =
            match x, y with
            | _ when (x = maxX) && (y = maxY) -> count
            | _ when (x > maxX) -> calcC2Count 1 (y + 1) count
            | _ -> calcC2Count (x + 1) y (count + calcC2CaseCount x y)
        let c1Count = 3 * maxX * maxY
        let c2Count = calcC2Count 1 1 0
        c1Count + c2Count

    [<TestCase(2, 2, 14, TimeThresholds.HardTimeLimit)>]
    [<TestCase(50, 50, 14234, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxX: int, maxY: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxX, maxY)