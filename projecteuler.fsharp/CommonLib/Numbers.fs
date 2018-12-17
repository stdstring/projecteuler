namespace CommonLib

open System

[<AbstractClass; Sealed>]
type Numbers =

    static member public GetDigits(number: int) =
        Numbers.GetDigits(number, 10)

    static member public GetDigits(number: int64) =
        Numbers.GetDigits(number, 10)

    static member public GetDigits(number: bigint) =
        Numbers.GetDigits(number, 10)

    static member public GetDigits(number: int, radix: int) =
        Numbers.CheckRadix(radix)
        match number with
        | _ when number < 0 -> raise (ArgumentOutOfRangeException("number"))
        | _ -> Numbers.GetDigitsImpl(number, radix, [])

    static member public GetDigits(number: int64, radix: int) =
        Numbers.CheckRadix(radix)
        match number with
        | _ when number < 0L -> raise (ArgumentOutOfRangeException("number"))
        | _ -> Numbers.GetDigitsImpl(number, radix, [])

    static member public GetDigits(number: bigint, radix: int) =
        Numbers.CheckRadix(radix)
        match number with
        | _ when number < 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ -> Numbers.GetDigitsImpl(number, radix, [])

    static member public GetNumber(digits: List<int>) =
        Numbers.GetNumber(digits, 10)

    static member public GetNumber(digits: List<int>, radix: int) =
        Numbers.CheckDigits(digits, radix)
        Numbers.CheckRadix(radix)
        let radixBig = bigint radix
        digits |> List.fold (fun number digit -> number * radixBig + (bigint digit)) 0I

    static member private CheckRadix(radix: int) =
        match radix with
        | _ when radix < 2 -> raise (ArgumentOutOfRangeException("radix"))
        | _ when (2 <= radix) && (radix <= 10) -> ()
        | _ -> raise (NotSupportedException())

    static member private CheckDigits(digits: List<int>, radix: int) =
        match (digits |> List.exists (fun digit -> (digit < 0) || (digit >= radix))) with
        | true -> raise (ArgumentOutOfRangeException("digits"))
        | false -> ()

    static member private GetDigitsImpl(number: int, radix: int, digits: List<int>) =
        match number with
        | _ when number < radix -> number :: digits
        | _ -> Numbers.GetDigitsImpl(number / radix, radix, (number % radix) :: digits)

    static member private GetDigitsImpl(number: int64, radix: int, digits: List<int>) =
        let radix64 = int64 radix
        match number with
        | _ when number < radix64 -> (int number) :: digits
        | _ -> Numbers.GetDigitsImpl(number / radix64, radix, (number % radix64 |> int) :: digits)

    static member private GetDigitsImpl(number: bigint, radix: int, digits: List<int>) =
        let radixBig = bigint radix
        match number with
        | _ when number < radixBig -> (int number) :: digits
        | _ -> Numbers.GetDigitsImpl(number / radixBig, radix, (number % radixBig |> int) :: digits)
