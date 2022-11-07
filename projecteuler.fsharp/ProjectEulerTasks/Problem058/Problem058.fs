namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils
open CommonLib

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