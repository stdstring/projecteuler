namespace CommonLib

open Rational
open System

module LinearEquationsSystem =

    let swapRows(matrixA: 'TValue[,], matrixB: 'TValue[], sourceRow: int, destRow: int) =
        // matrixA
        for column in {0 .. matrixA.GetLength(1) - 1} do
            let value = matrixA.[sourceRow, column]
            matrixA.[sourceRow, column] <- matrixA.[destRow, column]
            matrixA.[destRow, column] <- value
        // matrixB
        let value = matrixB.[sourceRow]
        matrixB.[sourceRow] <- matrixB.[destRow]
        matrixB.[destRow] <- value

    [<AbstractClass; Sealed>]
    type GaussSolver =

        static member public Solve(matrixA: RationalNumber32[,], matrixB: RationalNumber32[]) =
            if matrixA.GetLength(0) <> matrixA.GetLength(1) then
                raise (new ArgumentException("matrixA"))
            if matrixA.GetLength(0) <> matrixB.Length then
                raise (new ArgumentException("matrixB"))
            match GaussSolver.PrepareMatrix(matrixA, matrixB) with
            | false -> None
            | true -> GaussSolver.CalcResult(matrixA, matrixB) |> Some

        static member public Solve(matrixA: RationalNumber64[,], matrixB: RationalNumber64[]) =
            if matrixA.GetLength(0) <> matrixA.GetLength(1) then
                raise (new ArgumentException("matrixA"))
            if matrixA.GetLength(0) <> matrixB.Length then
                raise (new ArgumentException("matrixB"))
            match GaussSolver.PrepareMatrix(matrixA, matrixB) with
            | false -> None
            | true -> GaussSolver.CalcResult(matrixA, matrixB) |> Some

        static member public Solve(matrixA: RationalNumber[,], matrixB: RationalNumber[]) =
            if matrixA.GetLength(0) <> matrixA.GetLength(1) then
                raise (new ArgumentException("matrixA"))
            if matrixA.GetLength(0) <> matrixB.Length then
                raise (new ArgumentException("matrixB"))
            match GaussSolver.PrepareMatrix(matrixA, matrixB) with
            | false -> None
            | true -> GaussSolver.CalcResult(matrixA, matrixB) |> Some

        static member private PrepareMatrix(matrixA: RationalNumber32[,], matrixB: RationalNumber32[]) =
            let rec prepareImpl (current: int) =
                match current with
                | _ when current >= matrixA.GetLength(0) -> true
                | _ ->
                    match GaussSolver.FindSuitableRow(matrixA, current) with
                    | None -> false
                    | Some row when row = current ->
                        GaussSolver.AddRowToRest(matrixA, matrixB, current)
                        (current + 1) |> prepareImpl
                    | Some row ->
                        swapRows(matrixA, matrixB, current, row)
                        GaussSolver.AddRowToRest(matrixA, matrixB, current)
                        (current + 1) |> prepareImpl
            0 |> prepareImpl

        static member private PrepareMatrix(matrixA: RationalNumber64[,], matrixB: RationalNumber64[]) =
            let rec prepareImpl (current: int) =
                match current with
                | _ when current >= matrixA.GetLength(0) -> true
                | _ ->
                    match GaussSolver.FindSuitableRow(matrixA, current) with
                    | None -> false
                    | Some row when row = current ->
                        GaussSolver.AddRowToRest(matrixA, matrixB, current)
                        (current + 1) |> prepareImpl
                    | Some row ->
                        swapRows(matrixA, matrixB, current, row)
                        GaussSolver.AddRowToRest(matrixA, matrixB, current)
                        (current + 1) |> prepareImpl
            0 |> prepareImpl

        static member private PrepareMatrix(matrixA: RationalNumber[,], matrixB: RationalNumber[]) =
            let rec prepareImpl (current: int) =
                match current with
                | _ when current >= matrixA.GetLength(0) -> true
                | _ ->
                    match GaussSolver.FindSuitableRow(matrixA, current) with
                    | None -> false
                    | Some row when row = current ->
                        GaussSolver.AddRowToRest(matrixA, matrixB, current)
                        (current + 1) |> prepareImpl
                    | Some row ->
                        swapRows(matrixA, matrixB, current, row)
                        GaussSolver.AddRowToRest(matrixA, matrixB, current)
                        (current + 1) |> prepareImpl
            0 |> prepareImpl

        static member private CalcResult(matrixA: RationalNumber32[,], matrixB: RationalNumber32[]) =
            let result = RationalNumber32.Zero |> Array.create matrixB.Length
            for i in {result.Length - 1 .. -1 .. 0} do
                let value = matrixB.[i] - (seq {result.Length - 1 .. -1 .. i + 1} |> Seq.map (fun j -> matrixA.[i, j] * result.[j]) |> Seq.sum)
                result.[i] <- (value / matrixA.[i, i]).Simplify()
            result

        static member private CalcResult(matrixA: RationalNumber64[,], matrixB: RationalNumber64[]) =
            let result = RationalNumber64.Zero |> Array.create matrixB.Length
            for i in {result.Length - 1 .. -1 .. 0} do
                let value = matrixB.[i] - (seq {result.Length - 1 .. -1 .. i + 1} |> Seq.map (fun j -> matrixA.[i, j] * result.[j]) |> Seq.sum)
                result.[i] <- (value / matrixA.[i, i]).Simplify()
            result

        static member private CalcResult(matrixA: RationalNumber[,], matrixB: RationalNumber[]) =
            let result = RationalNumber.Zero |> Array.create matrixB.Length
            for i in {result.Length - 1 .. -1 .. 0} do
                let value = matrixB.[i] - (seq {result.Length - 1 .. -1 .. i + 1} |> Seq.map (fun j -> matrixA.[i, j] * result.[j]) |> Seq.sum)
                result.[i] <- (value / matrixA.[i, i]).Simplify()
            result

        static member private FindSuitableRow (matrixA: RationalNumber32[,], current: int) =
            let rec findImpl (row: int) =
                match row with
                | _ when row >= matrixA.GetLength(0) -> None
                | _ when matrixA.[row, current].IsZero |> not -> Some row
                | _ -> (row + 1) |> findImpl
            current |> findImpl

        static member private FindSuitableRow (matrixA: RationalNumber64[,], current: int) =
            let rec findImpl (row: int) =
                match row with
                | _ when row >= matrixA.GetLength(0) -> None
                | _ when matrixA.[row, current].IsZero |> not -> Some row
                | _ -> (row + 1) |> findImpl
            current |> findImpl

        static member private FindSuitableRow (matrixA: RationalNumber[,], current: int) =
            let rec findImpl (row: int) =
                match row with
                | _ when row >= matrixA.GetLength(0) -> None
                | _ when matrixA.[row, current].IsZero |> not -> Some row
                | _ -> (row + 1) |> findImpl
            current |> findImpl

        static member private AddRowToRest(matrixA: RationalNumber32[,], matrixB: RationalNumber32[], sourceRow: int) =
            for destRow in {sourceRow + 1 .. matrixA.GetLength(0) - 1} do
                let factor = (-matrixA.[destRow, sourceRow] / matrixA.[sourceRow, sourceRow]).Simplify()
                GaussSolver.AddRowWithFactor(matrixA, matrixB, sourceRow, destRow, factor)

        static member private AddRowToRest(matrixA: RationalNumber64[,], matrixB: RationalNumber64[], sourceRow: int) =
            for destRow in {sourceRow + 1 .. matrixA.GetLength(0) - 1} do
                let factor = (-matrixA.[destRow, sourceRow] / matrixA.[sourceRow, sourceRow]).Simplify()
                GaussSolver.AddRowWithFactor(matrixA, matrixB, sourceRow, destRow, factor)

        static member private AddRowToRest(matrixA: RationalNumber[,], matrixB: RationalNumber[], sourceRow: int) =
            for destRow in {sourceRow + 1 .. matrixA.GetLength(0) - 1} do
                let factor = (-matrixA.[destRow, sourceRow] / matrixA.[sourceRow, sourceRow]).Simplify()
                GaussSolver.AddRowWithFactor(matrixA, matrixB, sourceRow, destRow, factor)

        static member private AddRowWithFactor(matrixA: RationalNumber32[,], matrixB: RationalNumber32[], sourceRow: int, destRow: int, factor: RationalNumber32) =
            // matrixA
            for column in {sourceRow .. matrixA.GetLength(1) - 1} do
                matrixA.[destRow, column] <- (matrixA.[destRow, column] + factor * matrixA.[sourceRow, column]).Simplify()
            // matrixB
            matrixB.[destRow] <- (matrixB.[destRow] + factor * matrixB.[sourceRow]).Simplify()

        static member private AddRowWithFactor(matrixA: RationalNumber64[,], matrixB: RationalNumber64[], sourceRow: int, destRow: int, factor: RationalNumber64) =
            // matrixA
            for column in {sourceRow .. matrixA.GetLength(1) - 1} do
                matrixA.[destRow, column] <- (matrixA.[destRow, column] + factor * matrixA.[sourceRow, column]).Simplify()
            // matrixB
            matrixB.[destRow] <- (matrixB.[destRow] + factor * matrixB.[sourceRow]).Simplify()

        static member private AddRowWithFactor(matrixA: RationalNumber[,], matrixB: RationalNumber[], sourceRow: int, destRow: int, factor: RationalNumber) =
            // matrixA
            for column in {sourceRow .. matrixA.GetLength(1) - 1} do
                matrixA.[destRow, column] <- (matrixA.[destRow, column] + factor * matrixA.[sourceRow, column]).Simplify()
            // matrixB
            matrixB.[destRow] <- (matrixB.[destRow] + factor * matrixB.[sourceRow]).Simplify()