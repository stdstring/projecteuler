namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type NumbersDividersTests() =

    [<Test>]
    member public this.GetDividers() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(0) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], NumbersDividers.GetDividers(1))
        ClassicAssert.AreEqual([1; 2], NumbersDividers.GetDividers(2))
        ClassicAssert.AreEqual([1; 3], NumbersDividers.GetDividers(3))
        ClassicAssert.AreEqual([1; 2; 4], NumbersDividers.GetDividers(4))
        ClassicAssert.AreEqual([1; 5], NumbersDividers.GetDividers(5))
        ClassicAssert.AreEqual([1; 2; 3; 6], NumbersDividers.GetDividers(6))
        ClassicAssert.AreEqual([1; 7], NumbersDividers.GetDividers(7))
        ClassicAssert.AreEqual([1; 2; 4; 8], NumbersDividers.GetDividers(8))
        ClassicAssert.AreEqual([1; 3; 9], NumbersDividers.GetDividers(9))
        ClassicAssert.AreEqual([1; 2; 5; 10], NumbersDividers.GetDividers(10))
        ClassicAssert.AreEqual([1; 11], NumbersDividers.GetDividers(11))
        ClassicAssert.AreEqual([1; 2; 3; 4; 6; 12], NumbersDividers.GetDividers(12))
        ClassicAssert.AreEqual([1; 13], NumbersDividers.GetDividers(13))
        ClassicAssert.AreEqual([1; 2; 7; 14], NumbersDividers.GetDividers(14))
        ClassicAssert.AreEqual([1; 3; 5; 15], NumbersDividers.GetDividers(15))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(0L) |> ignore) |> ignore
        ClassicAssert.AreEqual([1L], NumbersDividers.GetDividers(1L))
        ClassicAssert.AreEqual([1L; 2L], NumbersDividers.GetDividers(2L))
        ClassicAssert.AreEqual([1L; 3L], NumbersDividers.GetDividers(3L))
        ClassicAssert.AreEqual([1L; 2L; 4L], NumbersDividers.GetDividers(4L))
        ClassicAssert.AreEqual([1L; 5L], NumbersDividers.GetDividers(5L))
        ClassicAssert.AreEqual([1L; 2L; 3L; 6L], NumbersDividers.GetDividers(6L))
        ClassicAssert.AreEqual([1L; 7L], NumbersDividers.GetDividers(7L))
        ClassicAssert.AreEqual([1L; 2L; 4L; 8L], NumbersDividers.GetDividers(8L))
        ClassicAssert.AreEqual([1L; 3L; 9L], NumbersDividers.GetDividers(9L))
        ClassicAssert.AreEqual([1L; 2L; 5L; 10L], NumbersDividers.GetDividers(10L))
        ClassicAssert.AreEqual([1L; 11L], NumbersDividers.GetDividers(11L))
        ClassicAssert.AreEqual([1L; 2L; 3L; 4L; 6L; 12L], NumbersDividers.GetDividers(12L))
        ClassicAssert.AreEqual([1L; 13L], NumbersDividers.GetDividers(13L))
        ClassicAssert.AreEqual([1L; 2L; 7L; 14L], NumbersDividers.GetDividers(14L))
        ClassicAssert.AreEqual([1L; 3L; 5L; 15L], NumbersDividers.GetDividers(15L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(0I) |> ignore) |> ignore
        ClassicAssert.AreEqual([1I], NumbersDividers.GetDividers(1I))
        ClassicAssert.AreEqual([1I; 2I], NumbersDividers.GetDividers(2I))
        ClassicAssert.AreEqual([1I; 3I], NumbersDividers.GetDividers(3I))
        ClassicAssert.AreEqual([1I; 2I; 4I], NumbersDividers.GetDividers(4I))
        ClassicAssert.AreEqual([1I; 5I], NumbersDividers.GetDividers(5I))
        ClassicAssert.AreEqual([1I; 2I; 3I; 6I], NumbersDividers.GetDividers(6I))
        ClassicAssert.AreEqual([1I; 7I], NumbersDividers.GetDividers(7I))
        ClassicAssert.AreEqual([1I; 2I; 4I; 8I], NumbersDividers.GetDividers(8I))
        ClassicAssert.AreEqual([1I; 3I; 9I], NumbersDividers.GetDividers(9I))
        ClassicAssert.AreEqual([1I; 2I; 5I; 10I], NumbersDividers.GetDividers(10I))
        ClassicAssert.AreEqual([1I; 11I], NumbersDividers.GetDividers(11I))
        ClassicAssert.AreEqual([1I; 2I; 3I; 4I; 6I; 12I], NumbersDividers.GetDividers(12I))
        ClassicAssert.AreEqual([1I; 13I], NumbersDividers.GetDividers(13I))
        ClassicAssert.AreEqual([1I; 2I; 7I; 14I], NumbersDividers.GetDividers(14I))
        ClassicAssert.AreEqual([1I; 3I; 5I; 15I], NumbersDividers.GetDividers(15I))

    [<Test>]
    member public this.GetPrimeDividers() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(0) |> ignore) |> ignore
        ClassicAssert.AreEqual(List.empty<int>, NumbersDividers.GetPrimeDividers(1))
        ClassicAssert.AreEqual([2], NumbersDividers.GetPrimeDividers(2))
        ClassicAssert.AreEqual([3], NumbersDividers.GetPrimeDividers(3))
        ClassicAssert.AreEqual([2], NumbersDividers.GetPrimeDividers(4))
        ClassicAssert.AreEqual([5], NumbersDividers.GetPrimeDividers(5))
        ClassicAssert.AreEqual([2; 3], NumbersDividers.GetPrimeDividers(6))
        ClassicAssert.AreEqual([7], NumbersDividers.GetPrimeDividers(7))
        ClassicAssert.AreEqual([2], NumbersDividers.GetPrimeDividers(8))
        ClassicAssert.AreEqual([3], NumbersDividers.GetPrimeDividers(9))
        ClassicAssert.AreEqual([2; 5], NumbersDividers.GetPrimeDividers(10))
        ClassicAssert.AreEqual([11], NumbersDividers.GetPrimeDividers(11))
        ClassicAssert.AreEqual([2; 3], NumbersDividers.GetPrimeDividers(12))
        ClassicAssert.AreEqual([13], NumbersDividers.GetPrimeDividers(13))
        ClassicAssert.AreEqual([2; 7], NumbersDividers.GetPrimeDividers(14))
        ClassicAssert.AreEqual([3; 5], NumbersDividers.GetPrimeDividers(15))
        ClassicAssert.AreEqual([2; 3; 5; 11; 13], NumbersDividers.GetPrimeDividers(2 * 2 * 3 * 3 * 3 * 5 * 5 * 11 * 13))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(0L) |> ignore) |> ignore
        ClassicAssert.AreEqual(List.empty<int64>, NumbersDividers.GetPrimeDividers(1L))
        ClassicAssert.AreEqual([2L], NumbersDividers.GetPrimeDividers(2L))
        ClassicAssert.AreEqual([3L], NumbersDividers.GetPrimeDividers(3L))
        ClassicAssert.AreEqual([2L], NumbersDividers.GetPrimeDividers(4L))
        ClassicAssert.AreEqual([5L], NumbersDividers.GetPrimeDividers(5L))
        ClassicAssert.AreEqual([2L; 3L], NumbersDividers.GetPrimeDividers(6L))
        ClassicAssert.AreEqual([7L], NumbersDividers.GetPrimeDividers(7L))
        ClassicAssert.AreEqual([2L], NumbersDividers.GetPrimeDividers(8L))
        ClassicAssert.AreEqual([3L], NumbersDividers.GetPrimeDividers(9L))
        ClassicAssert.AreEqual([2L; 5L], NumbersDividers.GetPrimeDividers(10L))
        ClassicAssert.AreEqual([11L], NumbersDividers.GetPrimeDividers(11L))
        ClassicAssert.AreEqual([2L; 3L], NumbersDividers.GetPrimeDividers(12L))
        ClassicAssert.AreEqual([13L], NumbersDividers.GetPrimeDividers(13L))
        ClassicAssert.AreEqual([2L; 7L], NumbersDividers.GetPrimeDividers(14L))
        ClassicAssert.AreEqual([3L; 5L], NumbersDividers.GetPrimeDividers(15L))
        ClassicAssert.AreEqual([2L; 3L; 5L; 11L; 13L], NumbersDividers.GetPrimeDividers(2L * 2L * 3L * 3L * 3L * 5L * 5L * 11L * 13L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(0I) |> ignore) |> ignore
        ClassicAssert.AreEqual(List.empty<bigint>, NumbersDividers.GetPrimeDividers(1I))
        ClassicAssert.AreEqual([2I], NumbersDividers.GetPrimeDividers(2I))
        ClassicAssert.AreEqual([3I], NumbersDividers.GetPrimeDividers(3I))
        ClassicAssert.AreEqual([2I], NumbersDividers.GetPrimeDividers(4I))
        ClassicAssert.AreEqual([5I], NumbersDividers.GetPrimeDividers(5I))
        ClassicAssert.AreEqual([2I; 3I], NumbersDividers.GetPrimeDividers(6I))
        ClassicAssert.AreEqual([7I], NumbersDividers.GetPrimeDividers(7I))
        ClassicAssert.AreEqual([2I], NumbersDividers.GetPrimeDividers(8I))
        ClassicAssert.AreEqual([3I], NumbersDividers.GetPrimeDividers(9I))
        ClassicAssert.AreEqual([2I; 5I], NumbersDividers.GetPrimeDividers(10I))
        ClassicAssert.AreEqual([11I], NumbersDividers.GetPrimeDividers(11I))
        ClassicAssert.AreEqual([2I; 3I], NumbersDividers.GetPrimeDividers(12I))
        ClassicAssert.AreEqual([13I], NumbersDividers.GetPrimeDividers(13I))
        ClassicAssert.AreEqual([2I; 7I], NumbersDividers.GetPrimeDividers(14I))
        ClassicAssert.AreEqual([3I; 5I], NumbersDividers.GetPrimeDividers(15I))
        ClassicAssert.AreEqual([2I; 3I; 5I; 11I; 13I], NumbersDividers.GetPrimeDividers(2I * 2I * 3I * 3I * 3I * 5I * 5I * 11I * 13I))

    [<Test>]
    member public this.IsPrime() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.IsPrime(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.IsPrime(0) |> ignore) |> ignore
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(1))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(2))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(3))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(4))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(5))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(6))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(7))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(8))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(9))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(10))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(11))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(12))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(13))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(14))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(15))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.IsPrime(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.IsPrime(0L) |> ignore) |> ignore
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(1L))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(2L))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(3L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(4L))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(5L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(6L))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(7L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(8L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(9L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(10L))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(11L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(12L))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(13L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(14L))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(15L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.IsPrime(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.IsPrime(0I) |> ignore) |> ignore
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(1I))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(2I))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(3I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(4I))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(5I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(6I))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(7I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(8I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(9I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(10I))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(11I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(12I))
        ClassicAssert.IsTrue(NumbersDividers.IsPrime(13I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(14I))
        ClassicAssert.IsFalse(NumbersDividers.IsPrime(15I))