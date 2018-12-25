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
