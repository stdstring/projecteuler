namespace CommonLib

open System

module Rational =

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
