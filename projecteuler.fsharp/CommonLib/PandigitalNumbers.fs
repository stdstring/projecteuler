namespace CommonLib

open System

module PandigitalNumbersDef =

    [<Literal>]
    let Pandigital2Min = 12L

    [<Literal>]
    let Pandigital2Max = 21L

    [<Literal>]
    let Pandigital3Min = 123L

    [<Literal>]
    let Pandigital3Max = 321L

    [<Literal>]
    let Pandigital4Min = 1234L

    [<Literal>]
    let Pandigital4Max = 4321L

    [<Literal>]
    let Pandigital5Min = 12345L

    [<Literal>]
    let Pandigital5Max = 54321L

    [<Literal>]
    let Pandigital6Min = 123456L

    [<Literal>]
    let Pandigital6Max = 654321L

    [<Literal>]
    let Pandigital7Min = 1234567L

    [<Literal>]
    let Pandigital7Max = 7654321L

    [<Literal>]
    let Pandigital8Min = 12345678L

    [<Literal>]
    let Pandigital8Max = 87654321L

    [<Literal>]
    let Pandigital9Min = 123456789L

    [<Literal>]
    let Pandigital9Max = 987654321L

    [<Literal>]
    let Pandigital10Min = 1023456789L

    [<Literal>]
    let Pandigital10Max = 9876543210L

[<AbstractClass; Sealed>]
type PandigitalNumbers =

    static member public IsPandigital(number: int) =
        number |> int64 |> PandigitalNumbers.IsPandigital

    static member public IsPandigital(number: int64) =
        let rec isPandigital (number: int64) (expectedValue: int64) (actualValue: int64) =
            match number with
            | 0L when expectedValue = actualValue -> true
            | 0L when expectedValue <> actualValue -> false
            | _ -> isPandigital (number / 10L) expectedValue (actualValue ||| (1L <<< ((number % 10L) |> int)))
        match number with
        | _ when number < 0L -> raise (ArgumentOutOfRangeException("number"))
        | _ when (PandigitalNumbersDef.Pandigital2Min <= number) && (number <= PandigitalNumbersDef.Pandigital2Max) -> isPandigital number 0b0000000110L 0L
        | _ when (PandigitalNumbersDef.Pandigital3Min <= number) && (number <= PandigitalNumbersDef.Pandigital3Max) -> isPandigital number 0b0000001110L 0L
        | _ when (PandigitalNumbersDef.Pandigital4Min <= number) && (number <= PandigitalNumbersDef.Pandigital4Max) -> isPandigital number 0b0000011110L 0L
        | _ when (PandigitalNumbersDef.Pandigital5Min <= number) && (number <= PandigitalNumbersDef.Pandigital5Max) -> isPandigital number 0b0000111110L 0L
        | _ when (PandigitalNumbersDef.Pandigital6Min <= number) && (number <= PandigitalNumbersDef.Pandigital6Max) -> isPandigital number 0b0001111110L 0L
        | _ when (PandigitalNumbersDef.Pandigital7Min <= number) && (number <= PandigitalNumbersDef.Pandigital7Max) -> isPandigital number 0b0011111110L 0L
        | _ when (PandigitalNumbersDef.Pandigital8Min <= number) && (number <= PandigitalNumbersDef.Pandigital8Max) -> isPandigital number 0b0111111110L 0L
        | _ when (PandigitalNumbersDef.Pandigital9Min <= number) && (number <= PandigitalNumbersDef.Pandigital9Max) -> isPandigital number 0b1111111110L 0L
        | _ when (PandigitalNumbersDef.Pandigital10Min <= number) && (number <= PandigitalNumbersDef.Pandigital10Max) -> isPandigital number 0b1111111111L 0L
        | _ -> false

    static member public IsPandigital(number: bigint) =
        number |> int64 |> PandigitalNumbers.IsPandigital