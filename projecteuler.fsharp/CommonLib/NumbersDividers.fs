namespace CommonLib

open System

[<AbstractClass; Sealed>]
type NumbersDividers =

    static member public GetDividers(number: int) =
        match number with
        | _ when number <= 0 -> raise (ArgumentOutOfRangeException("number"))
        | 1 -> [1]
        | _  -> number |> NumbersDividers.GetDividersImpl

    static member public GetDividers(number: int64) =
        match number with
        | _ when number <= 0L -> raise (ArgumentOutOfRangeException("number"))
        | 1L -> [1L]
        | _  -> number |> NumbersDividers.GetDividersImpl

    static member public GetDividers(number: bigint) =
        match number with
        | _ when number <= 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ when number = 1I -> [1I]
        | _  -> number |> NumbersDividers.GetDividersImpl

    static member public GetPrimeDividers(number: int) =
        match number with
        | _ when number <= 0 -> raise (ArgumentOutOfRangeException("number"))
        | 1 -> []
        | _  -> NumbersDividers.GetPrimeDividersImpl(number) |> List.rev

    static member public GetPrimeDividers(number: int64) =
        match number with
        | _ when number <= 0L -> raise (ArgumentOutOfRangeException("number"))
        | 1L -> []
        | _  -> NumbersDividers.GetPrimeDividersImpl(number) |> List.rev

    static member public GetPrimeDividers(number: bigint) =
        match number with
        | _ when number <= 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ when number = 1I -> []
        | _  -> NumbersDividers.GetPrimeDividersImpl(number) |> List.rev

    static member public IsPrime(number: int) =
        match number with
        | _ when number <= 0 -> raise (ArgumentOutOfRangeException("number"))
        | 1 -> false
        | 2 -> true
        | _ when number % 2 = 0 -> false
        | _ -> NumbersDividers.IsOddPrime(number, 3)

    static member public IsPrime(number: int64) =
        match number with
        | _ when number <= 0L -> raise (ArgumentOutOfRangeException("number"))
        | 1L -> false
        | 2L -> true
        | _ when number % 2L = 0L -> false
        | _ -> NumbersDividers.IsOddPrime(number, 3L)

    static member public IsPrime(number: bigint) =
        match number with
        | _ when number <= 0I -> raise (ArgumentOutOfRangeException("number"))
        | _ when number = 1I -> false
        | _ when number = 2I -> true
        | _ when number % 2I = 0I -> false
        | _ -> NumbersDividers.IsOddPrime(number, 3I)

    static member private GetDividersImpl(number: int) =
        let rec searchDividers (divider: int) (smallDividers: List<int>) (largeDividers: List<int>) =
            match divider with
            | 1 -> searchDividers 2 [1] [number]
            | _ when divider * divider > number -> (smallDividers |> List.rev) @ largeDividers
            | _ when divider * divider = number -> (smallDividers |> List.rev) @ (divider :: largeDividers)
            | _ when number % divider = 0 -> searchDividers (divider + 1) (divider :: smallDividers) (number / divider :: largeDividers)
            | _ -> searchDividers (divider + 1) smallDividers largeDividers
        searchDividers 1 [] []

    static member private GetDividersImpl(number: int64) =
        let rec searchDividers (divider: int64) (smallDividers: List<int64>) (largeDividers: List<int64>) =
            match divider with
            | 1L -> searchDividers 2L [1L] [number]
            | _ when divider * divider > number -> (smallDividers |> List.rev) @ largeDividers
            | _ when divider * divider = number -> (smallDividers |> List.rev) @ (divider :: largeDividers)
            | _ when number % divider = 0L -> searchDividers (divider + 1L) (divider :: smallDividers) (number / divider :: largeDividers)
            | _ -> searchDividers (divider + 1L) smallDividers largeDividers
        searchDividers 1L [] []

    static member private GetDividersImpl(number: bigint) =
        let rec searchDividers (divider: bigint) (smallDividers: List<bigint>) (largeDividers: List<bigint>) =
            match divider with
            | _ when divider = 1I -> searchDividers 2I [1I] [number]
            | _ when divider * divider > number -> (smallDividers |> List.rev) @ largeDividers
            | _ when divider * divider = number -> (smallDividers |> List.rev) @ (divider :: largeDividers)
            | _ when number % divider = 0I -> searchDividers (divider + 1I) (divider :: smallDividers) (number / divider :: largeDividers)
            | _ -> searchDividers (divider + 1I) smallDividers largeDividers
        searchDividers 1I [] []

    static member private GetPrimeDividersImpl(number: int) =
        let rec extractDivider (number: int)  (divider: int) =
            match number % divider with
            | 0 -> extractDivider (number / divider) divider
            | _ -> number
        let rec searchPrimeDividers (number: int) (divider: int) (dividers: List<int>) =
            match number with
            | 1 -> dividers
            | _ when divider * divider > number -> number :: dividers
            | _ when divider * divider = number -> divider :: dividers
            | _ when number % divider = 0 -> divider :: dividers |> searchPrimeDividers (extractDivider number divider) (divider + 2)
            | _ -> dividers |> searchPrimeDividers number (divider + 2)
        match number % 2 with
        | 0 -> [2] |> searchPrimeDividers (extractDivider number 2) 3
        | _ -> [] |> searchPrimeDividers number 3

    static member private GetPrimeDividersImpl(number: int64) =
        let rec extractDivider (number: int64)  (divider: int64) =
            match number % divider with
            | 0L -> extractDivider (number / divider) divider
            | _ -> number
        let rec searchPrimeDividers (number: int64) (divider: int64) (dividers: List<int64>) =
            match number with
            | 1L -> dividers
            | _ when divider * divider > number -> number :: dividers
            | _ when divider * divider = number -> divider :: dividers
            | _ when number % divider = 0L -> divider :: dividers |> searchPrimeDividers (extractDivider number divider) (divider + 2L)
            | _ -> dividers |> searchPrimeDividers number (divider + 2L)
        match number % 2L with
        | 0L -> [2L] |> searchPrimeDividers (extractDivider number 2L) 3L
        | _ -> [] |> searchPrimeDividers number 3L

    static member private GetPrimeDividersImpl(number: bigint) =
        let rec extractDivider (number: bigint)  (divider: bigint) =
            match number % divider with
            | rem when rem = 0I -> extractDivider (number / divider) divider
            | _ -> number
        let rec searchPrimeDividers (number: bigint) (divider: bigint) (dividers: List<bigint>) =
            match number with
            | _ when number = 1I -> dividers
            | _ when divider * divider > number -> number :: dividers
            | _ when divider * divider = number -> divider :: dividers
            | _ when number % divider = 0I -> divider :: dividers |> searchPrimeDividers (extractDivider number divider) (divider + 2I)
            | _ -> dividers |> searchPrimeDividers number (divider + 2I)
        match number % 2I with
        | rem when rem = 0I -> [2I] |> searchPrimeDividers (extractDivider number 2I) 3I
        | _ -> [] |> searchPrimeDividers number 3I

    static member private IsOddPrime(number: int, divider: int) =
        match number with
        | _ when divider * divider > number -> true
        | _ when divider * divider = number -> false
        | _ when number % divider = 0 -> false
        | _ -> NumbersDividers.IsOddPrime(number, divider + 2)

    static member private IsOddPrime(number: int64, divider: int64) =
        match number with
        | _ when divider * divider > number -> true
        | _ when divider * divider = number -> false
        | _ when number % divider = 0L -> false
        | _ -> NumbersDividers.IsOddPrime(number, divider + 2L)

    static member private IsOddPrime(number: bigint, divider: bigint) =
        match number with
        | _ when divider * divider > number -> true
        | _ when divider * divider = number -> false
        | _ when number % divider = 0I -> false
        | _ -> NumbersDividers.IsOddPrime(number, divider + 2I)