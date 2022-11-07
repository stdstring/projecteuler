namespace ProjectEulerTasks

open NUnit.Framework
open ProjectEulerTasks.Utils

// TODO (std_string) : think about solve via solution of the corresponding Pell/Diophantine equation
[<TestFixture>]
type Problem045() =

    let calcPentagonalNumber (n: int64) = n * (3L * n - 1L) / 2L

    let calcHexagonalNumber (n: int64) = n * (2L * n - 1L)

    // notes: for T(n) = n * (n + 1) / 2 exists substitution n = 2t - 1
    // T(n) = n * (n + 1) / 2 = (2t - 1) * (2t - 1 + 1) / 2 = (2t - 1) * 2t / 2 = t * (2t - 1) = H(t)
    // so we can exclude triangle numbers
    let rec processNumbers (pentagonalN: int64) (hexagonalN: int64) =
        let pentagonalNumber = pentagonalN |> calcPentagonalNumber
        let hexagonalNumber = hexagonalN |> calcHexagonalNumber
        match pentagonalNumber, hexagonalNumber with
        | _ when pentagonalNumber = hexagonalNumber -> hexagonalNumber
        | _ when pentagonalNumber < hexagonalNumber -> processNumbers (pentagonalN + 1L) hexagonalN
        | _ when pentagonalNumber > hexagonalNumber -> processNumbers pentagonalN (hexagonalN + 1L)
        | _ -> failwith "Unexpected branch of match expression"


    let solveImpl (_: int64) (pentagonalStart: int64) (hexagonalStart: int64) =
        processNumbers (pentagonalStart + 1L) (hexagonalStart + 1L)

    [<TestCase(1, 1, 1, 40755, TimeThresholds.HardTimeLimit)>]
    [<TestCase(285, 165, 143, 1533776805L, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(triangleStart: int, pentagonalStart: int, hexagonalStart: int, expectedAnswer: int64, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, triangleStart |> int64, pentagonalStart |> int64, hexagonalStart |> int64)
