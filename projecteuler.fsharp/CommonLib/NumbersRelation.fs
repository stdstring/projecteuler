namespace CommonLib

open Checked
open System

[<AbstractClass; Sealed>]
type NumbersRelation =

    static member public CalcGCD(a: int, b: int) =
        match a, b with
        | 0, 0 -> raise (ArgumentException("Both numbers are 0 simultaneously"))
        | a, b when a < 0 -> NumbersRelation.CalcGCD(-a, b)
        | a, b when b < 0 -> NumbersRelation.CalcGCD(a, -b)
        | a, b when a < b -> NumbersRelation.CalcGCD(b, a)
        | a, b when a = b -> a
        | a, 0 -> a
        | a, b -> NumbersRelation.CalcGCD(b, a % b)

    static member public CalcGCD(a: int64, b: int64) =
        match a, b with
        | 0L, 0L -> raise (ArgumentException("Both numbers are 0 simultaneously"))
        | a, b when a < 0L -> NumbersRelation.CalcGCD(-a, b)
        | a, b when b < 0L -> NumbersRelation.CalcGCD(a, -b)
        | a, b when a < b -> NumbersRelation.CalcGCD(b, a)
        | a, b when a = b -> a
        | a, 0L -> a
        | a, b -> NumbersRelation.CalcGCD(b, a % b)

    static member public CalcGCD(a: bigint, b: bigint) =
        match a, b with
        | a, b when (a = 0I) && (b = 0I) -> raise (ArgumentException("Both numbers are 0 simultaneously"))
        | a, b when a < 0I -> NumbersRelation.CalcGCD(-a, b)
        | a, b when b < 0I -> NumbersRelation.CalcGCD(a, -b)
        | a, b when a < b -> NumbersRelation.CalcGCD(b, a)
        | a, b when a = b -> a
        | a, b when b = 0I -> a
        | a, b -> NumbersRelation.CalcGCD(b, a % b)

    static member public CalcLCM(a: int, b: int) =
        let gcd = NumbersRelation.CalcGCD(a, b)
        let aDivider = a / gcd
        let bDivider = b / gcd
        aDivider * bDivider * gcd

    static member public CalcLCM(a: int64, b: int64) =
        let gcd = NumbersRelation.CalcGCD(a, b)
        let aDivider = a / gcd
        let bDivider = b / gcd
        aDivider * bDivider * gcd

    static member public CalcLCM(a: bigint, b: bigint) =
        (a * b) / NumbersRelation.CalcGCD(a, b)

    static member public IsMutuallySimple(a: int, b: int) =
        NumbersRelation.CalcGCD(a, b) = 1

    static member public IsMutuallySimple(a: int64, b: int64) =
        NumbersRelation.CalcGCD(a, b) = 1L

    static member public IsMutuallySimple(a: bigint, b: bigint) =
        NumbersRelation.CalcGCD(a, b) = 1I
