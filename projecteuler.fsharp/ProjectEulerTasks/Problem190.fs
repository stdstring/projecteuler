namespace ProjectEulerTasks

open CommonLib.LinearEquationsSystem
open CommonLib.Rational
open NUnit.Framework
open ProjectEulerTasks.Utils

// Let Sm = (x1, x2, ... , xm) be the m-tuple of positive real numbers with x1 + x2 + ... + xm = m for which Pm = x1 * x2^2 * ... * xm^m is maximised.
// For example, it can be verified that [P10] = 4112 ([ ] is the integer part function).
// Find Sum[Pm] for 2 <= m <= 15.

[<TestFixture>]
type Problem190() =

    let calcXTuple (m: int) =
        let size = m - 2 + 1
        let matrixA = Array2D.init size size (fun row column -> let k = row + 2 in (if row = column then k + 1 else k) |> RationalNumber32)
        let matrixB = seq{2 .. m} |> Seq.map (fun k -> RationalNumber32(k * m)) |> Seq.toArray
        match GaussSolver.Solve(matrixA, matrixB) with
        | None -> raise (System.InvalidOperationException())
        | Some xTupleRest ->
            let x1 = RationalNumber32(m) - (xTupleRest |> Seq.reduce(fun left right -> left + right))
            xTupleRest |> Seq.append [x1]

    let calcPm (xTuple: seq<RationalNumber32>) =
        let numerator = xTuple |> Seq.mapi (fun index xValue -> pown (xValue.Numerator |> bigint) (index + 1)) |> Seq.reduce (fun left right -> left * right)
        let denominator = xTuple |> Seq.mapi (fun index xValue -> pown (xValue.Denominator |> bigint) (index + 1)) |> Seq.reduce (fun left right -> left * right)
        (numerator / denominator) |> int

    let calcForM (m: int) = m |> calcXTuple |> calcPm

    let solveImpl (maxM: int) =
        // Description:
        // 1) x1 + x2 + ... + xm = m => x1 = m - x2 - ... - xm => Pm = (m - x2 - ... - xm) * x2^2 * ... * xm^m
        // 2) dPm / dxk = (x2^2 * ... * xm^m) * (-1 + k * (m - x2 - ... - xm) / xk)
        // 3) dPm / dxk = 0 => -1 + k * (m - x2 - ... - xm) / xk = 0 => k * x2 + ... + (k + 1) * xk + ... + k * xm = k * m - linear equation system.
        //    It is easy to see that solution of this linear equation system is the max point of Pm
        seq {2 .. maxM} |> Seq.map (fun m -> m |> calcForM) |> Seq.sum

    [<TestCase(15, 371048281, TimeThresholds.HardTimeLimit)>]
    member public this.Solve(maxM: int, expectedAnswer: int, timeLimit: int) =
        SolutionUtils.CheckSolution(timeLimit, expectedAnswer, solveImpl, maxM)