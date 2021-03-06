﻿namespace CommonLib

open Checked
open System

module Rational =

    // based on Int32 for numerator and denominator
    type RationalNumber32(numerator: int, denominator: int) =

        do if denominator = 0 then
            raise (ArgumentException("denominator"))

        // TODO (std_string) : think about name
        let rec simplify () =
            match numerator, denominator with
            | _ when numerator % denominator = 0 -> numerator / denominator, 1
            | _ when (denominator % numerator = 0) ->
                match denominator / numerator with
                | value when value > 0 -> 1, denominator / numerator
                | value when value < 0 -> -1, -denominator / numerator
                | _ -> failwith "Unexpected branch of match expression"
            | _ when denominator < 0 -> -numerator, -denominator
            | _ -> numerator, denominator

        let numerator, denominator = simplify ()

        new (number: int) =
            RationalNumber32(number, 1)

        member val Numerator = numerator with get
        member val Denominator = denominator with get

        member val IsZero = (numerator = 0) with get

        member val IsInteger = (denominator = 1) with get

        member public this.Reverse() =
            new RationalNumber32(denominator, numerator)

        member public this.Simplify() =
            let gcd = NumbersRelation.CalcGCD(numerator, denominator)
            new RationalNumber32(numerator / gcd, denominator / gcd)

        override this.Equals(obj: obj) =
            match obj with
            | :? RationalNumber32 as other -> (this.Numerator * other.Denominator - other.Numerator * this.Denominator) = 0
            | _ -> false

        override this.GetHashCode() =
            let gcd = NumbersRelation.CalcGCD(numerator, denominator)
            hash(numerator / gcd, denominator / gcd)

        interface IComparable with
            member this.CompareTo(obj: obj): int =
                let other = obj :?> RationalNumber32
                match this.Numerator * other.Denominator - other.Numerator * this.Denominator with
                | 0 -> 0
                | value when value < 0 -> -1
                | value when value > 0 -> 1
                | _ -> failwith "Unexpected branch of match expression"

        static member (+) (left: RationalNumber32, right: RationalNumber32) =
            match left.Denominator, right.Denominator with
            | _ when left.Denominator % right.Denominator = 0 -> new RationalNumber32(left.Numerator + right.Numerator * (left.Denominator / right.Denominator), left.Denominator)
            | _ when right.Denominator % left.Denominator = 0 -> new RationalNumber32(left.Numerator * (right.Denominator / left.Denominator) + right.Numerator, right.Denominator)
            | _ -> new RationalNumber32(left.Numerator * right.Denominator + right.Numerator * left.Denominator, left.Denominator * right.Denominator)

        static member (+) (left: RationalNumber32, right: int) =
            new RationalNumber32(left.Numerator + right * left.Denominator, left.Denominator)

        static member (+) (left: int, right: RationalNumber32) =
            new RationalNumber32(left * right.Denominator + right.Numerator, right.Denominator)

        static member (-) (left: RationalNumber32, right: RationalNumber32) =
            match left.Denominator, right.Denominator with
            | _ when left.Denominator % right.Denominator = 0 -> new RationalNumber32(left.Numerator - right.Numerator * (left.Denominator / right.Denominator), left.Denominator)
            | _ when right.Denominator % left.Denominator = 0 -> new RationalNumber32(left.Numerator * (right.Denominator / left.Denominator) - right.Numerator, right.Denominator)
            | _ -> new RationalNumber32(left.Numerator * right.Denominator - right.Numerator * left.Denominator, left.Denominator * right.Denominator)

        static member (-) (left: RationalNumber32, right: int) =
            new RationalNumber32(left.Numerator - right * left.Denominator, left.Denominator)

        static member (-) (left: int, right: RationalNumber32) =
            new RationalNumber32(left * right.Denominator - right.Numerator, right.Denominator)

        static member (*) (left: RationalNumber32, right: RationalNumber32) =
            new RationalNumber32(left.Numerator * right.Numerator, left.Denominator * right.Denominator)

        static member (*) (left: RationalNumber32, right: int) =
            new RationalNumber32(left.Numerator * right, left.Denominator)

        static member (*) (left: int, right: RationalNumber32) =
            new RationalNumber32(left * right.Numerator, right.Denominator)

        static member (/) (left: RationalNumber32, right: RationalNumber32) =
            new RationalNumber32(left.Numerator * right.Denominator, left.Denominator * right.Numerator)

        static member (/) (left: RationalNumber32, right: int) =
            new RationalNumber32(left.Numerator, left.Denominator * right)

        static member (/) (left: int, right: RationalNumber32) =
            new RationalNumber32(left * right.Denominator, right.Numerator)

        static member (~-) (number: RationalNumber32) =
            new RationalNumber32(-number.Numerator, number.Denominator)

        static member op_Equality (left: RationalNumber32, right: RationalNumber32) =
            (left.Numerator * right.Denominator - right.Numerator * left.Denominator) = 0

        static member op_LessThanOrEqual (left: RationalNumber32, right: RationalNumber32) =
            left <= right

        static member op_LessThan (left: RationalNumber32, right: RationalNumber32) =
            left < right

        static member op_GreaterThanOrEqual (left: RationalNumber32, right: RationalNumber32) =
            left >= right

        static member op_GreaterThan (left: RationalNumber32, right: RationalNumber32) =
            left > right

        static member Zero = RationalNumber32(0)

        static member One = RationalNumber32(1)


    // based on Int64 for numerator and denominator
    type RationalNumber64(numerator: int64, denominator: int64) =

        do if denominator = 0L then
            raise (ArgumentException("denominator"))

        // TODO (std_string) : think about name
        let rec simplify () =
            match numerator, denominator with
            | _ when numerator % denominator = 0L -> numerator / denominator, 1L
            | _ when (denominator % numerator = 0L) ->
                match denominator / numerator with
                | value when value > 0L -> 1L, denominator / numerator
                | value when value < 0L -> -1L, -denominator / numerator
                | _ -> failwith "Unexpected branch of match expression"
            | _ when denominator < 0L -> -numerator, -denominator
            | _ -> numerator, denominator

        let numerator, denominator = simplify ()

        new (number: int64) =
            RationalNumber64(number, 1L)

        new (numerator: int, denominator: int) =
            RationalNumber64(numerator |> int64, denominator |> int64)

        new (number: int) =
            RationalNumber64(number |> int64)

        member val Numerator = numerator with get
        member val Denominator = denominator with get

        member val IsZero = (numerator = 0L) with get

        member val IsInteger = (denominator = 1L) with get

        member public this.Reverse() =
            new RationalNumber64(denominator, numerator)

        member public this.Simplify() =
            let gcd = NumbersRelation.CalcGCD(numerator, denominator)
            new RationalNumber64(numerator / gcd, denominator / gcd)

        override this.Equals(obj: obj) =
            match obj with
            | :? RationalNumber64 as other -> (this.Numerator * other.Denominator - other.Numerator * this.Denominator) = 0L
            | _ -> false

        override this.GetHashCode() =
            let gcd = NumbersRelation.CalcGCD(numerator, denominator)
            hash(numerator / gcd, denominator / gcd)

        interface IComparable with
            member this.CompareTo(obj: obj): int =
                let other = obj :?> RationalNumber64
                match this.Numerator * other.Denominator - other.Numerator * this.Denominator with
                | 0L -> 0
                | value when value < 0L -> -1
                | value when value > 0L -> 1
                | _ -> failwith "Unexpected branch of match expression"

        static member (+) (left: RationalNumber64, right: RationalNumber64) =
            match left.Denominator, right.Denominator with
            | _ when left.Denominator % right.Denominator = 0L -> new RationalNumber64(left.Numerator + right.Numerator * (left.Denominator / right.Denominator), left.Denominator)
            | _ when right.Denominator % left.Denominator = 0L -> new RationalNumber64(left.Numerator * (right.Denominator / left.Denominator) + right.Numerator, right.Denominator)
            | _ -> new RationalNumber64(left.Numerator * right.Denominator + right.Numerator * left.Denominator, left.Denominator * right.Denominator)

        static member (+) (left: RationalNumber64, right: int64) =
            new RationalNumber64(left.Numerator + right * left.Denominator, left.Denominator)

        static member (+) (left: RationalNumber64, right: int) =
            left + (right |> int64)

        static member (+) (left: int64, right: RationalNumber64) =
            new RationalNumber64(left * right.Denominator + right.Numerator, right.Denominator)

        static member (+) (left: int, right: RationalNumber64) =
            (left |> int64) + right

        static member (-) (left: RationalNumber64, right: RationalNumber64) =
            match left.Denominator, right.Denominator with
            | _ when left.Denominator % right.Denominator = 0L -> new RationalNumber64(left.Numerator - right.Numerator * (left.Denominator / right.Denominator), left.Denominator)
            | _ when right.Denominator % left.Denominator = 0L -> new RationalNumber64(left.Numerator * (right.Denominator / left.Denominator) - right.Numerator, right.Denominator)
            | _ -> new RationalNumber64(left.Numerator * right.Denominator - right.Numerator * left.Denominator, left.Denominator * right.Denominator)

        static member (-) (left: RationalNumber64, right: int64) =
            new RationalNumber64(left.Numerator - right * left.Denominator, left.Denominator)

        static member (-) (left: RationalNumber64, right: int) =
            left - (right |> int64)

        static member (-) (left: int64, right: RationalNumber64) =
            new RationalNumber64(left * right.Denominator - right.Numerator, right.Denominator)

        static member (-) (left: int, right: RationalNumber64) =
            (left |> int64) - right

        static member (*) (left: RationalNumber64, right: RationalNumber64) =
            new RationalNumber64(left.Numerator * right.Numerator, left.Denominator * right.Denominator)

        static member (*) (left: RationalNumber64, right: int64) =
            new RationalNumber64(left.Numerator * right, left.Denominator)

        static member (*) (left: RationalNumber64, right: int) =
            left * (right |> int64)

        static member (*) (left: int64, right: RationalNumber64) =
            new RationalNumber64(left * right.Numerator, right.Denominator)

        static member (*) (left: int, right: RationalNumber64) =
            (left |> int64) * right

        static member (/) (left: RationalNumber64, right: RationalNumber64) =
            new RationalNumber64(left.Numerator * right.Denominator, left.Denominator * right.Numerator)

        static member (/) (left: RationalNumber64, right: int64) =
            new RationalNumber64(left.Numerator, left.Denominator * right)

        static member (/) (left: RationalNumber64, right: int) =
            left / (right |> int64)

        static member (/) (left: int64, right: RationalNumber64) =
            new RationalNumber64(left * right.Denominator, right.Numerator)

        static member (/) (left: int, right: RationalNumber64) =
            (left |> int64) / right

        static member (~-) (number: RationalNumber64) =
            new RationalNumber64(-number.Numerator, number.Denominator)

        static member op_Equality (left: RationalNumber64, right: RationalNumber64) =
            (left.Numerator * right.Denominator - right.Numerator * left.Denominator) = 0L

        static member op_LessThanOrEqual (left: RationalNumber64, right: RationalNumber64) =
            left <= right

        static member op_LessThan (left: RationalNumber64, right: RationalNumber64) =
            left < right

        static member op_GreaterThanOrEqual (left: RationalNumber64, right: RationalNumber64) =
            left >= right

        static member op_GreaterThan (left: RationalNumber64, right: RationalNumber64) =
            left > right

        static member Zero = RationalNumber64(0L)

        static member One = RationalNumber64(1L)

    // based on BigInteger for numerator and denominator
    type RationalNumber(numerator: bigint, denominator: bigint) =

        do if denominator = 0I then
            raise (ArgumentException("denominator"))

        // TODO (std_string) : think about name
        let rec simplify () =
            match numerator, denominator with
            | _ when numerator % denominator = 0I -> numerator / denominator, 1I
            | _ when (denominator % numerator = 0I) ->
                match denominator / numerator with
                | value when value > 0I -> 1I, denominator / numerator
                | value when value < 0I -> -1I, -denominator / numerator
                | _ -> failwith "Unexpected branch of match expression"
            | _ when denominator < 0I -> -numerator, -denominator
            | _ -> numerator, denominator

        let numerator, denominator = simplify ()

        new (number: bigint) =
            RationalNumber(number, 1I)

        new (numerator: int64, denominator: int64) =
            RationalNumber(numerator |> bigint, denominator |> bigint)

        new (number: int64) =
            RationalNumber(number |> bigint)

        new (numerator: int, denominator: int) =
            RationalNumber(numerator |> bigint, denominator |> bigint)

        new (number: int) =
            RationalNumber(number |> bigint)

        member val Numerator = numerator with get
        member val Denominator = denominator with get

        member val IsZero = (numerator = 0I) with get

        member val IsInteger = (denominator = 1I) with get

        member public this.Reverse() =
            new RationalNumber(denominator, numerator)

        member public this.Simplify() =
            let gcd = NumbersRelation.CalcGCD(numerator, denominator)
            new RationalNumber(numerator / gcd, denominator / gcd)

        override this.Equals(obj: obj) =
            match obj with
            | :? RationalNumber as other -> (this.Numerator * other.Denominator - other.Numerator * this.Denominator) = 0I
            | _ -> false

        override this.GetHashCode() =
            let gcd = NumbersRelation.CalcGCD(numerator, denominator)
            hash(numerator / gcd, denominator / gcd)

        interface IComparable with
            member this.CompareTo(obj: obj): int =
                let other = obj :?> RationalNumber
                match this.Numerator * other.Denominator - other.Numerator * this.Denominator with
                | value when value < 0I -> -1
                | value when value = 0I -> 0
                | value when value > 0I -> 1
                | _ -> failwith "Unexpected branch of match expression"

        static member (+) (left: RationalNumber, right: RationalNumber) =
            match left.Denominator, right.Denominator with
            | _ when left.Denominator % right.Denominator = 0I -> new RationalNumber(left.Numerator + right.Numerator * (left.Denominator / right.Denominator), left.Denominator)
            | _ when right.Denominator % left.Denominator = 0I -> new RationalNumber(left.Numerator * (right.Denominator / left.Denominator) + right.Numerator, right.Denominator)
            | _ -> new RationalNumber(left.Numerator * right.Denominator + right.Numerator * left.Denominator, left.Denominator * right.Denominator)

        static member (+) (left: RationalNumber, right: bigint) =
            new RationalNumber(left.Numerator + right * left.Denominator, left.Denominator)

        static member (+) (left: RationalNumber, right: int64) =
            left + (right |> bigint)

        static member (+) (left: RationalNumber, right: int) =
            left + (right |> bigint)

        static member (+) (left: bigint, right: RationalNumber) =
            new RationalNumber(left * right.Denominator + right.Numerator, right.Denominator)

        static member (+) (left: int64, right: RationalNumber) =
            (left |> bigint) + right

        static member (+) (left: int, right: RationalNumber) =
            (left |> bigint) + right

        static member (-) (left: RationalNumber, right: RationalNumber) =
            match left.Denominator, right.Denominator with
            | _ when left.Denominator % right.Denominator = 0I -> new RationalNumber(left.Numerator - right.Numerator * (left.Denominator / right.Denominator), left.Denominator)
            | _ when right.Denominator % left.Denominator = 0I -> new RationalNumber(left.Numerator * (right.Denominator / left.Denominator) - right.Numerator, right.Denominator)
            | _ -> new RationalNumber(left.Numerator * right.Denominator - right.Numerator * left.Denominator, left.Denominator * right.Denominator)

        static member (-) (left: RationalNumber, right: bigint) =
            new RationalNumber(left.Numerator - right * left.Denominator, left.Denominator)

        static member (-) (left: RationalNumber, right: int64) =
            left - (right |> bigint)

        static member (-) (left: RationalNumber, right: int) =
            left - (right |> bigint)

        static member (-) (left: bigint, right: RationalNumber) =
            new RationalNumber(left * right.Denominator - right.Numerator, right.Denominator)

        static member (-) (left: int64, right: RationalNumber) =
            (left |> bigint) - right

        static member (-) (left: int, right: RationalNumber) =
            (left |> bigint) - right

        static member (*) (left: RationalNumber, right: RationalNumber) =
            new RationalNumber(left.Numerator * right.Numerator, left.Denominator * right.Denominator)

        static member (*) (left: RationalNumber, right: bigint) =
            new RationalNumber(left.Numerator * right, left.Denominator)

        static member (*) (left: RationalNumber, right: int64) =
            left * (right |> bigint)

        static member (*) (left: RationalNumber, right: int) =
            left * (right |> bigint)

        static member (*) (left: bigint, right: RationalNumber) =
            new RationalNumber(left * right.Numerator, right.Denominator)

        static member (*) (left: int64, right: RationalNumber) =
            (left |> bigint) * right

        static member (*) (left: int, right: RationalNumber) =
            (left |> bigint) * right

        static member (/) (left: RationalNumber, right: RationalNumber) =
            new RationalNumber(left.Numerator * right.Denominator, left.Denominator * right.Numerator)

        static member (/) (left: RationalNumber, right: bigint) =
            new RationalNumber(left.Numerator, left.Denominator * right)

        static member (/) (left: RationalNumber, right: int64) =
            left / (right |> bigint)

        static member (/) (left: RationalNumber, right: int) =
            left / (right |> bigint)

        static member (/) (left: bigint, right: RationalNumber) =
            new RationalNumber(left * right.Denominator, right.Numerator)

        static member (/) (left: int64, right: RationalNumber) =
            (left |> bigint) / right

        static member (/) (left: int, right: RationalNumber) =
            (left |> bigint) / right

        static member (~-) (number: RationalNumber) =
            new RationalNumber(-number.Numerator, number.Denominator)

        static member op_Equality (left: RationalNumber, right: RationalNumber) =
            (left.Numerator * right.Denominator - right.Numerator * left.Denominator) = 0I

        static member op_LessThanOrEqual (left: RationalNumber, right: RationalNumber) =
            left <= right

        static member op_LessThan (left: RationalNumber, right: RationalNumber) =
            left < right

        static member op_GreaterThanOrEqual (left: RationalNumber, right: RationalNumber) =
            left >= right

        static member op_GreaterThan (left: RationalNumber, right: RationalNumber) =
            left > right

        static member Zero = RationalNumber(0I)

        static member One = RationalNumber(1I)