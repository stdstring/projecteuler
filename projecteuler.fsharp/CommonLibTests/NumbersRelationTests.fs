namespace CommonLibTests

open System
open NUnit.Framework
open CommonLib

[<TestFixture>]
type NumbersRelationTests() =

    let maxint = Int32.MaxValue
    let minint = Int32.MinValue
    let maxint64 = Int64.MaxValue
    let minint64 = Int64.MinValue

    [<Test>]
    member public this.CalcGCD() =
        // int
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcGCD(0, 0) |> ignore) |> ignore
        Assert.AreEqual(3, NumbersRelation.CalcGCD(-15, 9))
        Assert.AreEqual(3, NumbersRelation.CalcGCD(15, -9))
        Assert.AreEqual(3, NumbersRelation.CalcGCD(9, 15))
        Assert.AreEqual(3, NumbersRelation.CalcGCD(3, 0))
        Assert.AreEqual(3, NumbersRelation.CalcGCD(3, 3))
        Assert.AreEqual(1, NumbersRelation.CalcGCD(5, 3))
        Assert.AreEqual(3, NumbersRelation.CalcGCD(15, 9))
        Assert.AreEqual(1, NumbersRelation.CalcGCD(21, 20))
        Assert.AreEqual(6, NumbersRelation.CalcGCD(24, 18))
        Assert.AreEqual(1, NumbersRelation.CalcGCD(maxint, maxint - 1))
        Assert.AreEqual(1, NumbersRelation.CalcGCD(minint - 1, minint - 2))
        // int64
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcGCD(0L, 0L) |> ignore) |> ignore
        Assert.AreEqual(3L, NumbersRelation.CalcGCD(-15L, 9L))
        Assert.AreEqual(3L, NumbersRelation.CalcGCD(15L, -9L))
        Assert.AreEqual(3L, NumbersRelation.CalcGCD(9L, 15L))
        Assert.AreEqual(3L, NumbersRelation.CalcGCD(3L, 0L))
        Assert.AreEqual(3L, NumbersRelation.CalcGCD(3L, 3L))
        Assert.AreEqual(1L, NumbersRelation.CalcGCD(5L, 3L))
        Assert.AreEqual(3L, NumbersRelation.CalcGCD(15L, 9L))
        Assert.AreEqual(1L, NumbersRelation.CalcGCD(21L, 20L))
        Assert.AreEqual(6L, NumbersRelation.CalcGCD(24L, 18L))
        Assert.AreEqual(1L, NumbersRelation.CalcGCD(maxint64, maxint64 - 1L))
        Assert.AreEqual(1L, NumbersRelation.CalcGCD(minint64 - 1L, minint64 - 2L))
        // bigint
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcGCD(0I, 0I) |> ignore) |> ignore
        Assert.AreEqual(3I, NumbersRelation.CalcGCD(-15I, 9I))
        Assert.AreEqual(3I, NumbersRelation.CalcGCD(15I, -9I))
        Assert.AreEqual(3I, NumbersRelation.CalcGCD(9I, 15I))
        Assert.AreEqual(3I, NumbersRelation.CalcGCD(3I, 0I))
        Assert.AreEqual(3I, NumbersRelation.CalcGCD(3I, 3I))
        Assert.AreEqual(1I, NumbersRelation.CalcGCD(5I, 3I))
        Assert.AreEqual(3I, NumbersRelation.CalcGCD(15I, 9I))
        Assert.AreEqual(1I, NumbersRelation.CalcGCD(21I, 20I))
        Assert.AreEqual(6I, NumbersRelation.CalcGCD(24I, 18I))
        Assert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(maxint), bigint(maxint - 1)))
        Assert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(minint - 1), bigint(minint - 2)))
        Assert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(maxint64), bigint(maxint64 - 1L)))
        Assert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(minint64 - 1L), bigint(minint64 - 2L)))

    [<Test>]
    member public this.CalcLCM() =
        // int
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcLCM(0, 0) |> ignore) |> ignore
        Assert.AreEqual(-45, NumbersRelation.CalcLCM(-15, 9))
        Assert.AreEqual(-45, NumbersRelation.CalcLCM(15, -9))
        Assert.AreEqual(45, NumbersRelation.CalcLCM(9, 15))
        Assert.AreEqual(0, NumbersRelation.CalcLCM(3, 0))
        Assert.AreEqual(3, NumbersRelation.CalcLCM(3, 3))
        Assert.AreEqual(15, NumbersRelation.CalcLCM(5, 3))
        Assert.AreEqual(45, NumbersRelation.CalcLCM(15, 9))
        Assert.AreEqual(420, NumbersRelation.CalcLCM(21, 20))
        Assert.AreEqual(72, NumbersRelation.CalcLCM(24, 18))
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(maxint, maxint - 1) |> ignore) |> ignore
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(minint - 1, minint - 2) |> ignore) |> ignore
        // int64
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcLCM(0L, 0L) |> ignore) |> ignore
        Assert.AreEqual(-45L, NumbersRelation.CalcLCM(-15L, 9L))
        Assert.AreEqual(-45L, NumbersRelation.CalcLCM(15L, -9L))
        Assert.AreEqual(45L, NumbersRelation.CalcLCM(9L, 15L))
        Assert.AreEqual(0L, NumbersRelation.CalcLCM(3L, 0L))
        Assert.AreEqual(3L, NumbersRelation.CalcLCM(3L, 3L))
        Assert.AreEqual(15L, NumbersRelation.CalcLCM(5L, 3L))
        Assert.AreEqual(45L, NumbersRelation.CalcLCM(15L, 9L))
        Assert.AreEqual(420L, NumbersRelation.CalcLCM(21L, 20L))
        Assert.AreEqual(72L, NumbersRelation.CalcLCM(24L, 18L))
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(maxint64, maxint64 - 1L) |> ignore) |> ignore
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(minint64 - 1L, minint64 - 2L) |> ignore) |> ignore
        // bigint
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcLCM(0I, 0I) |> ignore) |> ignore
        Assert.AreEqual(-45I, NumbersRelation.CalcLCM(-15I, 9I))
        Assert.AreEqual(-45I, NumbersRelation.CalcLCM(15I, -9I))
        Assert.AreEqual(45I, NumbersRelation.CalcLCM(9I, 15I))
        Assert.AreEqual(0I, NumbersRelation.CalcLCM(3I, 0I))
        Assert.AreEqual(3I, NumbersRelation.CalcLCM(3I, 3I))
        Assert.AreEqual(15I, NumbersRelation.CalcLCM(5I, 3I))
        Assert.AreEqual(45I, NumbersRelation.CalcLCM(15I, 9I))
        Assert.AreEqual(420I, NumbersRelation.CalcLCM(21I, 20I))
        Assert.AreEqual(72I, NumbersRelation.CalcLCM(24I, 18I))
        Assert.AreEqual(bigint(maxint) * bigint(maxint - 1), NumbersRelation.CalcLCM(bigint(maxint), bigint(maxint - 1)))
        Assert.AreEqual(bigint(minint - 1) * bigint(minint - 2), NumbersRelation.CalcLCM(bigint(minint - 1), bigint(minint - 2)))
        Assert.AreEqual(bigint(maxint64) * bigint(maxint64 - 1L), NumbersRelation.CalcLCM(bigint(maxint64), bigint(maxint64 - 1L)))
        Assert.AreEqual(bigint(minint64 - 1L) * bigint(minint64 - 2L), NumbersRelation.CalcLCM(bigint(minint64 - 1L), bigint(minint64 - 2L)))

    [<Test>]
    member public this.IsMutuallySimple() =
        // int
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.IsMutuallySimple(0, 0) |> ignore) |> ignore
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(-15, 9))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(15, -9))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(9, 15))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(3, 0))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(3, 3))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(5, 3))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(15, 9))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(21, 20))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(24, 18))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(maxint, maxint - 1))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(minint - 1, minint - 2))
        // int64
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.IsMutuallySimple(0L, 0L) |> ignore) |> ignore
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(-15L, 9L))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(15L, -9L))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(9L, 15L))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(3L, 0L))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(3L, 3L))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(5L, 3L))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(15L, 9L))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(21L, 20L))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(24L, 18L))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(maxint64, maxint64 - 1L))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(minint64 - 1L, minint64 - 2L))
        // bigint
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.IsMutuallySimple(0I, 0I) |> ignore) |> ignore
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(-15I, 9I))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(15I, -9I))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(9I, 15I))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(3I, 0I))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(3I, 3I))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(5I, 3I))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(15I, 9I))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(21I, 20I))
        Assert.IsFalse(NumbersRelation.IsMutuallySimple(24I, 18I))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(maxint), bigint(maxint - 1)))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(minint - 1), bigint(minint - 2)))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(maxint64), bigint(maxint64 - 1L)))
        Assert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(minint64 - 1L), bigint(minint64 - 2L)))
