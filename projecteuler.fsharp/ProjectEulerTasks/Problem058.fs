namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

// Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
//
// 37 36 35 34 33 32 31
// 38 17 16 15 14 13 30
// 39 18  5  4  3 12 29
// 40 19  6  1  2 11 28
// 41 20  7  8  9 10 27
// 42 21 22 23 24 25 26
// 43 44 45 46 47 48 49
//
// It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
// If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
// If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

type Problem058() =

    let solveImpl (primesRatio: float) =
        // Notes:
        // Let S(N - 1) - last number from previous layer, L(N) - side size for layer N
        // N1 = S(N - 1) + L(N) - 1, N2 =  N1 + L(N) - 1, N3 =  N2 + L(N) - 1, S(N) = N4 =  N3 + L(N) - 1
        let rec processLayer (prevNumber: int) (prevSideSize: int) (primesCount: int) (totalCount: int) =
            match primesCount with
            | _ when (primesCount > 0) && ((float primesCount) / (float totalCount) < primesRatio) -> prevSideSize
            | _ ->
                let sideSize = prevSideSize + 2
                let n1 = prevNumber + sideSize - 1
                let n2 = n1 + sideSize - 1
                let n3 = n2 + sideSize - 1
                let n4 = n3 + sideSize - 1
                processLayer n4 sideSize (primesCount + ([n1; n2; n3; n4] |> List.filter (fun number -> NumbersDividers.IsPrime(number)) |> List.length)) (totalCount + 4)
        processLayer 1 1 0 1

    [<TestCase(0.1, 26241, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(primesRatio: float, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, primesRatio)