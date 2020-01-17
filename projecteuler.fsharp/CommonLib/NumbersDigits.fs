namespace CommonLib

open System

[<AbstractClass; Sealed>]
type NumbersDigits =

    static member public GetDigits(number: int) =
        NumbersDigits.GetDigits(number, 10)

    static member public GetDigits(number: int64) =
        NumbersDigits.GetDigits(number, 10)

    static member public GetDigits(number: bigint) =
        NumbersDigits.GetDigits(number, 10)

    static member public GetDigits(number: int, radix: int) =
        NumbersDigits.CheckRadix(radix)
        match number with
        | _ when number < 0 -> raise (ArgumentOutOfRangeException("number"))
        | _ -> NumbersDigits.GetDigitsImpl(number, radix, [])

    static member public GetDigits(number: int64, radix: int) =
        NumbersDigits.CheckRadix(radix)
        match number with
        | _ when number < 0L -> raise (ArgumentOutOfRangeException("number"))
        | _ -> NumbersDigits.GetDigitsImpl(number, radix, [])

    static member public GetDigits(number: bigint, radix: int) =
        NumbersDigits.CheckRadix(radix)
        match number with
        | _ when number < 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ -> NumbersDigits.GetDigitsImpl(number, radix, [])

    static member public GetFixedSizeDigits(number: int, size: int) =
        NumbersDigits.GetFixedSizeDigits(number, 10, size)

    static member public GetFixedSizeDigits(number: int64, size: int) =
        NumbersDigits.GetFixedSizeDigits(number, 10, size)

    static member public GetFixedSizeDigits(number: bigint, size: int) =
        NumbersDigits.GetFixedSizeDigits(number, 10, size)

    static member public GetFixedSizeDigits(number: int, radix: int, size: int) =
        NumbersDigits.CheckRadix(radix)
        NumbersDigits.CheckSize(size)
        match number with
        | _ when number < 0 -> raise (ArgumentOutOfRangeException("number"))
        | _ -> NumbersDigits.FixDigitsSize(NumbersDigits.GetDigitsImpl(number, radix, []), size)

    static member public GetFixedSizeDigits(number: int64, radix: int, size: int) =
        NumbersDigits.CheckRadix(radix)
        NumbersDigits.CheckSize(size)
        match number with
        | _ when number < 0L -> raise (ArgumentOutOfRangeException("number"))
        | _ -> NumbersDigits.FixDigitsSize(NumbersDigits.GetDigitsImpl(number, radix, []), size)

    static member public GetFixedSizeDigits(number: bigint, radix: int, size: int) =
        NumbersDigits.CheckRadix(radix)
        NumbersDigits.CheckSize(size)
        match number with
        | _ when number < 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ -> NumbersDigits.FixDigitsSize(NumbersDigits.GetDigitsImpl(number, radix, []), size)

    static member public GetNumber(digits: List<int>) =
        NumbersDigits.GetNumber(digits, 10)

    static member public GetNumber(digits: List<int>, radix: int) =
        NumbersDigits.CheckDigits(digits, radix)
        NumbersDigits.CheckRadix(radix)
        let radixBig = bigint radix
        digits |> List.fold (fun number digit -> number * radixBig + (bigint digit)) 0I

    static member private CheckRadix(radix: int) =
        match radix with
        | _ when radix < 2 -> raise (ArgumentOutOfRangeException("radix"))
        | _ when (2 <= radix) && (radix <= 10) -> ()
        | _ -> raise (NotSupportedException())

    static member private CheckSize(size: int) =
        match size with
        | _ when size < 1 -> raise (ArgumentOutOfRangeException("radix"))
        | _ -> ()

    static member private CheckDigits(digits: List<int>, radix: int) =
        match (digits |> List.exists (fun digit -> (digit < 0) || (digit >= radix))) with
        | true -> raise (ArgumentOutOfRangeException("digits"))
        | false -> ()

    static member private GetDigitsImpl(number: int, radix: int, digits: List<int>) =
        match number with
        | _ when number < radix -> number :: digits
        | _ -> NumbersDigits.GetDigitsImpl(number / radix, radix, (number % radix) :: digits)

    static member private GetDigitsImpl(number: int64, radix: int, digits: List<int>) =
        let radix64 = int64 radix
        match number with
        | _ when number < radix64 -> (int number) :: digits
        | _ -> NumbersDigits.GetDigitsImpl(number / radix64, radix, (number % radix64 |> int) :: digits)

    static member private GetDigitsImpl(number: bigint, radix: int, digits: List<int>) =
        let radixBig = bigint radix
        match number with
        | _ when number < radixBig -> (int number) :: digits
        | _ -> NumbersDigits.GetDigitsImpl(number / radixBig, radix, (number % radixBig |> int) :: digits)

    static member private FixDigitsSize(digits: List<int>, size: int) =
        match size with
        | _ when size > digits.Length -> List.append (List.init (size - digits.Length) (fun _ -> 0)) digits
        | _ -> digits
