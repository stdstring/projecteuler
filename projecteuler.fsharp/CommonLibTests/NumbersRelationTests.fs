namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type NumbersRelationTests() =

    let maxint = Int32.MaxValue
    let minint = Int32.MinValue
    let maxint64 = Int64.MaxValue
    let minint64 = Int64.MinValue

    [<Test>]
    member public this.CalcGCD() =
        // int
        ClassicAssert.AreEqual(0,  NumbersRelation.CalcGCD(0, 0))
        ClassicAssert.AreEqual(3, NumbersRelation.CalcGCD(-15, 9))
        ClassicAssert.AreEqual(3, NumbersRelation.CalcGCD(15, -9))
        ClassicAssert.AreEqual(3, NumbersRelation.CalcGCD(9, 15))
        ClassicAssert.AreEqual(3, NumbersRelation.CalcGCD(3, 0))
        ClassicAssert.AreEqual(3, NumbersRelation.CalcGCD(3, 3))
        ClassicAssert.AreEqual(1, NumbersRelation.CalcGCD(5, 3))
        ClassicAssert.AreEqual(3, NumbersRelation.CalcGCD(15, 9))
        ClassicAssert.AreEqual(1, NumbersRelation.CalcGCD(21, 20))
        ClassicAssert.AreEqual(6, NumbersRelation.CalcGCD(24, 18))
        ClassicAssert.AreEqual(1, NumbersRelation.CalcGCD(maxint, maxint - 1))
        ClassicAssert.AreEqual(1, NumbersRelation.CalcGCD(minint - 1, minint - 2))
        // int64
        ClassicAssert.AreEqual(0L,  NumbersRelation.CalcGCD(0L, 0L))
        ClassicAssert.AreEqual(3L, NumbersRelation.CalcGCD(-15L, 9L))
        ClassicAssert.AreEqual(3L, NumbersRelation.CalcGCD(15L, -9L))
        ClassicAssert.AreEqual(3L, NumbersRelation.CalcGCD(9L, 15L))
        ClassicAssert.AreEqual(3L, NumbersRelation.CalcGCD(3L, 0L))
        ClassicAssert.AreEqual(3L, NumbersRelation.CalcGCD(3L, 3L))
        ClassicAssert.AreEqual(1L, NumbersRelation.CalcGCD(5L, 3L))
        ClassicAssert.AreEqual(3L, NumbersRelation.CalcGCD(15L, 9L))
        ClassicAssert.AreEqual(1L, NumbersRelation.CalcGCD(21L, 20L))
        ClassicAssert.AreEqual(6L, NumbersRelation.CalcGCD(24L, 18L))
        ClassicAssert.AreEqual(1L, NumbersRelation.CalcGCD(maxint64, maxint64 - 1L))
        ClassicAssert.AreEqual(1L, NumbersRelation.CalcGCD(minint64 - 1L, minint64 - 2L))
        // bigint
        ClassicAssert.AreEqual(0I,  NumbersRelation.CalcGCD(0I, 0I))
        ClassicAssert.AreEqual(3I, NumbersRelation.CalcGCD(-15I, 9I))
        ClassicAssert.AreEqual(3I, NumbersRelation.CalcGCD(15I, -9I))
        ClassicAssert.AreEqual(3I, NumbersRelation.CalcGCD(9I, 15I))
        ClassicAssert.AreEqual(3I, NumbersRelation.CalcGCD(3I, 0I))
        ClassicAssert.AreEqual(3I, NumbersRelation.CalcGCD(3I, 3I))
        ClassicAssert.AreEqual(1I, NumbersRelation.CalcGCD(5I, 3I))
        ClassicAssert.AreEqual(3I, NumbersRelation.CalcGCD(15I, 9I))
        ClassicAssert.AreEqual(1I, NumbersRelation.CalcGCD(21I, 20I))
        ClassicAssert.AreEqual(6I, NumbersRelation.CalcGCD(24I, 18I))
        ClassicAssert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(maxint), bigint(maxint - 1)))
        ClassicAssert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(minint - 1), bigint(minint - 2)))
        ClassicAssert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(maxint64), bigint(maxint64 - 1L)))
        ClassicAssert.AreEqual(1I, NumbersRelation.CalcGCD(bigint(minint64 - 1L), bigint(minint64 - 2L)))

    [<Test>]
    member public this.CalcLCM() =
        // int
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcLCM(0, 0) |> ignore) |> ignore
        ClassicAssert.AreEqual(-45, NumbersRelation.CalcLCM(-15, 9))
        ClassicAssert.AreEqual(-45, NumbersRelation.CalcLCM(15, -9))
        ClassicAssert.AreEqual(45, NumbersRelation.CalcLCM(9, 15))
        ClassicAssert.AreEqual(0, NumbersRelation.CalcLCM(3, 0))
        ClassicAssert.AreEqual(3, NumbersRelation.CalcLCM(3, 3))
        ClassicAssert.AreEqual(15, NumbersRelation.CalcLCM(5, 3))
        ClassicAssert.AreEqual(45, NumbersRelation.CalcLCM(15, 9))
        ClassicAssert.AreEqual(420, NumbersRelation.CalcLCM(21, 20))
        ClassicAssert.AreEqual(72, NumbersRelation.CalcLCM(24, 18))
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(maxint, maxint - 1) |> ignore) |> ignore
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(minint - 1, minint - 2) |> ignore) |> ignore
        // int64
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcLCM(0L, 0L) |> ignore) |> ignore
        ClassicAssert.AreEqual(-45L, NumbersRelation.CalcLCM(-15L, 9L))
        ClassicAssert.AreEqual(-45L, NumbersRelation.CalcLCM(15L, -9L))
        ClassicAssert.AreEqual(45L, NumbersRelation.CalcLCM(9L, 15L))
        ClassicAssert.AreEqual(0L, NumbersRelation.CalcLCM(3L, 0L))
        ClassicAssert.AreEqual(3L, NumbersRelation.CalcLCM(3L, 3L))
        ClassicAssert.AreEqual(15L, NumbersRelation.CalcLCM(5L, 3L))
        ClassicAssert.AreEqual(45L, NumbersRelation.CalcLCM(15L, 9L))
        ClassicAssert.AreEqual(420L, NumbersRelation.CalcLCM(21L, 20L))
        ClassicAssert.AreEqual(72L, NumbersRelation.CalcLCM(24L, 18L))
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(maxint64, maxint64 - 1L) |> ignore) |> ignore
        Assert.Throws<OverflowException>(fun() -> NumbersRelation.CalcLCM(minint64 - 1L, minint64 - 2L) |> ignore) |> ignore
        // bigint
        Assert.Throws<ArgumentException>(fun() -> NumbersRelation.CalcLCM(0I, 0I) |> ignore) |> ignore
        ClassicAssert.AreEqual(-45I, NumbersRelation.CalcLCM(-15I, 9I))
        ClassicAssert.AreEqual(-45I, NumbersRelation.CalcLCM(15I, -9I))
        ClassicAssert.AreEqual(45I, NumbersRelation.CalcLCM(9I, 15I))
        ClassicAssert.AreEqual(0I, NumbersRelation.CalcLCM(3I, 0I))
        ClassicAssert.AreEqual(3I, NumbersRelation.CalcLCM(3I, 3I))
        ClassicAssert.AreEqual(15I, NumbersRelation.CalcLCM(5I, 3I))
        ClassicAssert.AreEqual(45I, NumbersRelation.CalcLCM(15I, 9I))
        ClassicAssert.AreEqual(420I, NumbersRelation.CalcLCM(21I, 20I))
        ClassicAssert.AreEqual(72I, NumbersRelation.CalcLCM(24I, 18I))
        ClassicAssert.AreEqual(bigint(maxint) * bigint(maxint - 1), NumbersRelation.CalcLCM(bigint(maxint), bigint(maxint - 1)))
        ClassicAssert.AreEqual(bigint(minint - 1) * bigint(minint - 2), NumbersRelation.CalcLCM(bigint(minint - 1), bigint(minint - 2)))
        ClassicAssert.AreEqual(bigint(maxint64) * bigint(maxint64 - 1L), NumbersRelation.CalcLCM(bigint(maxint64), bigint(maxint64 - 1L)))
        ClassicAssert.AreEqual(bigint(minint64 - 1L) * bigint(minint64 - 2L), NumbersRelation.CalcLCM(bigint(minint64 - 1L), bigint(minint64 - 2L)))

    [<Test>]
    member public this.IsMutuallySimple() =
        // int
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(0, 0))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(-15, 9))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(15, -9))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(9, 15))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(3, 0))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(3, 3))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(5, 3))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(15, 9))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(21, 20))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(24, 18))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(maxint, maxint - 1))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(minint - 1, minint - 2))
        // int64
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(0L, 0L))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(-15L, 9L))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(15L, -9L))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(9L, 15L))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(3L, 0L))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(3L, 3L))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(5L, 3L))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(15L, 9L))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(21L, 20L))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(24L, 18L))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(maxint64, maxint64 - 1L))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(minint64 - 1L, minint64 - 2L))
        // bigint
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(0I, 0I))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(-15I, 9I))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(15I, -9I))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(9I, 15I))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(3I, 0I))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(3I, 3I))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(5I, 3I))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(15I, 9I))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(21I, 20I))
        ClassicAssert.IsFalse(NumbersRelation.IsMutuallySimple(24I, 18I))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(maxint), bigint(maxint - 1)))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(minint - 1), bigint(minint - 2)))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(maxint64), bigint(maxint64 - 1L)))
        ClassicAssert.IsTrue(NumbersRelation.IsMutuallySimple(bigint(minint64 - 1L), bigint(minint64 - 2L)))
