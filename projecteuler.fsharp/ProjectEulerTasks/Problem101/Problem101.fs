namespace ProjectEulerTasks

open CommonLib.LinearEquationsSystem
open CommonLib.Rational
open NUnit.Framework
open ProjectEulerTasks.Utils

[<TestFixture>]
type Problem101() =

    let generateMatrixA (maxN: int) (maxPower: int) =
        let matrixA = Array2D.create maxN (maxPower + 1) 0L
        for n in {1 .. maxN} do
            let row = n - 1
            for power in {0 .. maxPower} do
                let column = power
                matrixA.[row, column] <- pown (n |> int64) power
        matrixA

    let generateMatrixB (polynom: int64 -> int64) (maxN: int) =
        [|for n in 1L .. maxN |> int64 -> n |> polynom|]

    let getPolynomFactors (polynom: int64 -> int64) (maxN: int) (maxPower: int) =
        let matrixA = generateMatrixA maxN maxPower |> Array2D.map RationalNumber64
        let matrixB = generateMatrixB polynom maxN |> Array.map RationalNumber64
        match GaussSolver.Solve(matrixA, matrixB) with
        | Some result ->
            match result |> Array.exists (fun number -> number.IsInteger |> not) with
            | false -> result |> Array.map (fun number -> number.Numerator)
            | true -> failwith "Unexpected result"
        | None -> failwith "Unexpected result"

    let calcFIT (polynomFactors: int64[]) (n: int) =
        polynomFactors |> Array.fold (fun (value, power) factor -> value + power * factor, power * (n |> int64)) (0L, 1L) |> fst

    let solveImpl (polynom: int64 -> int64) (maxPower: int) =
        seq {0 .. maxPower - 1} |> Seq.map (fun power -> let n = power + 1 in (n + 1) |> calcFIT (getPolynomFactors polynom n power)) |> Seq.sum

    [<Test>]
    member public this.SolveExample() =
        let expectedAnswer = 74L
        let polynom = fun n -> pown n 3
        SolutionUtils.CheckSolution(TimeThresholds.HardTimeLimit, expectedAnswer, solveImpl, polynom, 3)

    [<Test>]
    member public this.SolveRealTask() =
        let expectedAnswer = 37076114526L
        let polynom = fun n -> (pown n 10) - (pown n 9) + (pown n 8) - (pown n 7) + (pown n 6) - (pown n 5) + (pown n 4) - (pown n 3) + (n * n) - n + 1L
        SolutionUtils.CheckSolution(TimeThresholds.HardTimeLimit, expectedAnswer, solveImpl, polynom, 10)