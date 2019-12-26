namespace CommonLib

open System

[<AbstractClass; Sealed>]
type Numbers =

    static member public CalcFactorial(number: int) =
        number |> bigint |> Numbers.CalcFactorial

    static member public CalcFactorial(number: int64) =
        number |> bigint |> Numbers.CalcFactorial

    static member public CalcFactorial(number: bigint) =
        match number with
        | _ when number < 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ when number = 0I -> 1I
        | _ -> seq {1I .. number} |> Seq.fold (fun product number -> product * number) 1I

    static member public CalcBinomialCoeff(n: int, k: int) =
        Numbers.CalcBinomialCoeff(n |> bigint, k |> bigint)

    static member public CalcBinomialCoeff(n: int64, k: int64) =
        Numbers.CalcBinomialCoeff(n |> bigint, k |> bigint)

    static member public CalcBinomialCoeff(n: bigint, k: bigint) =
        match n, k with
        | _, _ when n < 0I -> raise (ArgumentOutOfRangeException("n"))
        | _, _ when k < 0I -> 0I
        | _, _ when k > n -> 0I
        | _, _ -> (seq{n - k + 1I .. n} |> Seq.fold (fun product value -> product * value) 1I) / Numbers.CalcFactorial(k)

    static member public IsPerfectSquare(number: int) =
        match number with
        | _ when number < 0 -> raise (ArgumentOutOfRangeException("number"))
        | 0 | 1 -> true
        | _ ->
            let divider = number |> float |> sqrt |> int
            match divider with
            | _ when divider * divider = number -> true
            | _ when divider * divider < number -> Numbers.IsPerfectSquare(number, divider, 1, (>))
            | _ when divider * divider > number -> Numbers.IsPerfectSquare(number, divider, -1, (<))
            | _ -> failwith "Unexpected branch of match expression"

    static member public IsPerfectSquare(number: int64) =
        match number with
        | _ when number < 0L -> raise (ArgumentOutOfRangeException("number"))
        | 0L | 1L -> true
        | _ ->
            let divider = number |> float |> sqrt |> int64
            match divider with
            | _ when divider * divider = number -> true
            | _ when divider * divider < number -> Numbers.IsPerfectSquare(number, divider, 1L, (>))
            | _ when divider * divider > number -> Numbers.IsPerfectSquare(number, divider, -1L, (<))
            | _ -> failwith "Unexpected branch of match expression"

    static member public IsPerfectSquare(number: bigint) =
        match number with
        | _ when number < 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ when (number =  0I) || (number = 1I) -> true
        | _ ->
            let divider = number |> float |> sqrt |> bigint
            match divider with
            | _ when divider * divider = number -> true
            | _ when divider * divider < number -> Numbers.IsPerfectSquare(number, divider, 1I, (>))
            | _ when divider * divider > number -> Numbers.IsPerfectSquare(number, divider, -1I, (<))
            | _ -> failwith "Unexpected branch of match expression"

    static member private IsPerfectSquare(number: int, divider: int, delta: int, op: int->int->bool) =
        let dividerSquare = divider * divider
        match dividerSquare with
        | _ when dividerSquare = number -> true
        | _ when op dividerSquare number -> false
        | _ -> Numbers.IsPerfectSquare(number, divider + delta, delta, op)

    static member private IsPerfectSquare(number: int64, divider: int64, delta: int64, op: int64->int64->bool) =
        let dividerSquare = divider * divider
        match dividerSquare with
        | _ when dividerSquare = number -> true
        | _ when op dividerSquare number -> false
        | _ -> Numbers.IsPerfectSquare(number, divider + delta, delta, op)

    static member private IsPerfectSquare(number: bigint, divider: bigint, delta: bigint, op: bigint->bigint->bool) =
        let dividerSquare = divider * divider
        match dividerSquare with
        | _ when dividerSquare = number -> true
        | _ when op dividerSquare number -> false
        | _ -> Numbers.IsPerfectSquare(number, divider + delta, delta, op)
