namespace CommonLibTests

open CommonLib.LinearEquationsSystem
open CommonLib.Rational
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type GaussSolverTests() =

    [<Test>]
    member public this.SolveBadArgs() =
        // RationalNumber32
        Assert.Throws<ArgumentException>(fun() -> GaussSolver.Solve(RationalNumber32.Zero |> Array2D.create 3 4, RationalNumber32.Zero |> Array.create 3) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> GaussSolver.Solve(RationalNumber32.Zero |> Array2D.create 3 3, RationalNumber32.Zero |> Array.create 4) |> ignore) |> ignore
        // RationalNumber64
        Assert.Throws<ArgumentException>(fun() -> GaussSolver.Solve(RationalNumber64.Zero |> Array2D.create 3 4, RationalNumber64.Zero |> Array.create 3) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> GaussSolver.Solve(RationalNumber64.Zero |> Array2D.create 3 3, RationalNumber64.Zero |> Array.create 4) |> ignore) |> ignore
        // RationalNumber
        Assert.Throws<ArgumentException>(fun() -> GaussSolver.Solve(RationalNumber.Zero |> Array2D.create 3 4, RationalNumber.Zero |> Array.create 3) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> GaussSolver.Solve(RationalNumber.Zero |> Array2D.create 3 3, RationalNumber.Zero |> Array.create 4) |> ignore) |> ignore

    [<Test>]
    member public this.SolveSystemWithotSolution() =
        // RationalNumber32
        this.CheckSolutionMiss(array2D [[1; 1]; [2; 2]], [|1; 2|])
        this.CheckSolutionMiss(array2D [[1; 0]; [2; 0]], [|1; 2|])
        // RationalNumber64
        this.CheckSolutionMiss(array2D [[1L; 1L]; [2L; 2L]], [|1L; 2L|])
        this.CheckSolutionMiss(array2D [[1L; 0L]; [2L; 0L]], [|1L; 2L|])
        // RationalNumber
        this.CheckSolutionMiss(array2D [[1I; 1I]; [2I; 2I]], [|1I; 2I|])
        this.CheckSolutionMiss(array2D [[1I; 0I]; [2I; 0I]], [|1I; 2I|])

    [<Test>]
    member public this.Solve() =
        // RationalNumber32
        this.CheckSolution(array2D [[1; -2; 1]; [2; 2; -1]; [4; -1; 1]], [|0; 3; 5|], [|1; 2; 3|])
        this.CheckSolution(array2D [[3; 2; 1; 1]; [1; -1; 4; -1]; [-2; -2; -3; 1]; [1; 5; -1; 2]], [|-2; -1; 9; 4|], [|-3; -1; 2; 7|])
        let matrixA = array2D [[RationalNumber32(3, 25); RationalNumber32(9, 50); RationalNumber32(-17, 100)];
                               [RationalNumber32(3, 50); RationalNumber32(9, 100); RationalNumber32(3, 20)];
                               [RationalNumber32(11, 50); RationalNumber32(-1, 10); RationalNumber32(3, 50)]]
        let matrixB = [|RationalNumber32(11, 2); RationalNumber32(-39, 20); RationalNumber32(1, 2)|]
        ClassicAssert.AreEqual([|RationalNumber32(10); RationalNumber32(5); RationalNumber32(-20)|] |> Some, GaussSolver.Solve(matrixA, matrixB))
        // RationalNumber64
        this.CheckSolution(array2D [[1L; -2L; 1L]; [2L; 2L; -1L]; [4L; -1L; 1L]], [|0L; 3L; 5L|], [|1L; 2L; 3L|])
        this.CheckSolution(array2D [[3L; 2L; 1L; 1L]; [1L; -1L; 4L; -1L]; [-2L; -2L; -3L; 1L]; [1L; 5L; -1L; 2L]], [|-2L; -1L; 9L; 4L|], [|-3L; -1L; 2L; 7L|])
        let matrixA = array2D [[RationalNumber64(3L, 25L); RationalNumber64(9L, 50L); RationalNumber64(-17L, 100L)];
                               [RationalNumber64(3L, 50L); RationalNumber64(9L, 100L); RationalNumber64(3L, 20L)];
                               [RationalNumber64(11L, 50L); RationalNumber64(-1L, 10L); RationalNumber64(3L, 50L)]]
        let matrixB = [|RationalNumber64(11L, 2L); RationalNumber64(-39L, 20L); RationalNumber64(1L, 2L)|]
        ClassicAssert.AreEqual([|RationalNumber64(10L); RationalNumber64(5L); RationalNumber64(-20L)|] |> Some, GaussSolver.Solve(matrixA, matrixB))
        // RationalNumber
        this.CheckSolution(array2D [[1I; -2I; 1I]; [2I; 2I; -1I]; [4I; -1I; 1I]], [|0I; 3I; 5I|], [|1I; 2I; 3I|])
        this.CheckSolution(array2D [[3I; 2I; 1I; 1I]; [1I; -1I; 4I; -1I]; [-2I; -2I; -3I; 1I]; [1I; 5I; -1I; 2I]], [|-2I; -1I; 9I; 4I|], [|-3I; -1I; 2I; 7I|])
        let matrixA = array2D [[RationalNumber(3I, 25I); RationalNumber(9I, 50I); RationalNumber(-17I, 100I)];
                               [RationalNumber(3I, 50I); RationalNumber(9I, 100I); RationalNumber(3I, 20I)];
                               [RationalNumber(11I, 50I); RationalNumber(-1I, 10I); RationalNumber(3I, 50I)]]
        let matrixB = [|RationalNumber(11I, 2I); RationalNumber(-39I, 20I); RationalNumber(1I, 2I)|]
        ClassicAssert.AreEqual([|RationalNumber(10I); RationalNumber(5I); RationalNumber(-20I)|] |> Some, GaussSolver.Solve(matrixA, matrixB))

    member public this.CheckSolution(matrixA: int32[,], matrixB: int32[], expectedResult: int32[]) =
        ClassicAssert.AreEqual(expectedResult |> Array.map RationalNumber32 |> Some, GaussSolver.Solve(matrixA |> Array2D.map RationalNumber32, matrixB |> Array.map RationalNumber32))

    member public this.CheckSolutionMiss(matrixA: int32[,], matrixB: int32[]) =
        ClassicAssert.AreEqual(None, GaussSolver.Solve(matrixA |> Array2D.map RationalNumber32, matrixB |> Array.map RationalNumber32))

    member public this.CheckSolution(matrixA: int64[,], matrixB: int64[], expectedResult: int64[]) =
        ClassicAssert.AreEqual(expectedResult |> Array.map RationalNumber64 |> Some, GaussSolver.Solve(matrixA |> Array2D.map RationalNumber64, matrixB |> Array.map RationalNumber64))

    member public this.CheckSolutionMiss(matrixA: int64[,], matrixB: int64[]) =
        ClassicAssert.AreEqual(None, GaussSolver.Solve(matrixA |> Array2D.map RationalNumber64, matrixB |> Array.map RationalNumber64))

    member public this.CheckSolution(matrixA: bigint[,], matrixB: bigint[], expectedResult: bigint[]) =
        ClassicAssert.AreEqual(expectedResult |> Array.map RationalNumber |> Some, GaussSolver.Solve(matrixA |> Array2D.map RationalNumber, matrixB |> Array.map RationalNumber))

    member public this.CheckSolutionMiss(matrixA: bigint[,], matrixB: bigint[]) =
        ClassicAssert.AreEqual(None, GaussSolver.Solve(matrixA |> Array2D.map RationalNumber, matrixB |> Array.map RationalNumber))