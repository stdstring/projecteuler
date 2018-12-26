namespace CommonLibTests

open NUnit.Framework
open CommonLib
open System

[<TestFixture>]
type NumbersDividersTests() =

    [<Test>]
    member public this.GetDividers() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(0) |> ignore) |> ignore
        Assert.AreEqual([1], NumbersDividers.GetDividers(1))
        Assert.AreEqual([1; 2], NumbersDividers.GetDividers(2))
        Assert.AreEqual([1; 3], NumbersDividers.GetDividers(3))
        Assert.AreEqual([1; 2; 4], NumbersDividers.GetDividers(4))
        Assert.AreEqual([1; 5], NumbersDividers.GetDividers(5))
        Assert.AreEqual([1; 2; 3; 6], NumbersDividers.GetDividers(6))
        Assert.AreEqual([1; 7], NumbersDividers.GetDividers(7))
        Assert.AreEqual([1; 2; 4; 8], NumbersDividers.GetDividers(8))
        Assert.AreEqual([1; 3; 9], NumbersDividers.GetDividers(9))
        Assert.AreEqual([1; 2; 5; 10], NumbersDividers.GetDividers(10))
        Assert.AreEqual([1; 11], NumbersDividers.GetDividers(11))
        Assert.AreEqual([1; 2; 3; 4; 6; 12], NumbersDividers.GetDividers(12))
        Assert.AreEqual([1; 13], NumbersDividers.GetDividers(13))
        Assert.AreEqual([1; 2; 7; 14], NumbersDividers.GetDividers(14))
        Assert.AreEqual([1; 3; 5; 15], NumbersDividers.GetDividers(15))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(0L) |> ignore) |> ignore
        Assert.AreEqual([1L], NumbersDividers.GetDividers(1L))
        Assert.AreEqual([1L; 2L], NumbersDividers.GetDividers(2L))
        Assert.AreEqual([1L; 3L], NumbersDividers.GetDividers(3L))
        Assert.AreEqual([1L; 2L; 4L], NumbersDividers.GetDividers(4L))
        Assert.AreEqual([1L; 5L], NumbersDividers.GetDividers(5L))
        Assert.AreEqual([1L; 2L; 3L; 6L], NumbersDividers.GetDividers(6L))
        Assert.AreEqual([1L; 7L], NumbersDividers.GetDividers(7L))
        Assert.AreEqual([1L; 2L; 4L; 8L], NumbersDividers.GetDividers(8L))
        Assert.AreEqual([1L; 3L; 9L], NumbersDividers.GetDividers(9L))
        Assert.AreEqual([1L; 2L; 5L; 10L], NumbersDividers.GetDividers(10L))
        Assert.AreEqual([1L; 11L], NumbersDividers.GetDividers(11L))
        Assert.AreEqual([1L; 2L; 3L; 4L; 6L; 12L], NumbersDividers.GetDividers(12L))
        Assert.AreEqual([1L; 13L], NumbersDividers.GetDividers(13L))
        Assert.AreEqual([1L; 2L; 7L; 14L], NumbersDividers.GetDividers(14L))
        Assert.AreEqual([1L; 3L; 5L; 15L], NumbersDividers.GetDividers(15L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetDividers(0I) |> ignore) |> ignore
        Assert.AreEqual([1I], NumbersDividers.GetDividers(1I))
        Assert.AreEqual([1I; 2I], NumbersDividers.GetDividers(2I))
        Assert.AreEqual([1I; 3I], NumbersDividers.GetDividers(3I))
        Assert.AreEqual([1I; 2I; 4I], NumbersDividers.GetDividers(4I))
        Assert.AreEqual([1I; 5I], NumbersDividers.GetDividers(5I))
        Assert.AreEqual([1I; 2I; 3I; 6I], NumbersDividers.GetDividers(6I))
        Assert.AreEqual([1I; 7I], NumbersDividers.GetDividers(7I))
        Assert.AreEqual([1I; 2I; 4I; 8I], NumbersDividers.GetDividers(8I))
        Assert.AreEqual([1I; 3I; 9I], NumbersDividers.GetDividers(9I))
        Assert.AreEqual([1I; 2I; 5I; 10I], NumbersDividers.GetDividers(10I))
        Assert.AreEqual([1I; 11I], NumbersDividers.GetDividers(11I))
        Assert.AreEqual([1I; 2I; 3I; 4I; 6I; 12I], NumbersDividers.GetDividers(12I))
        Assert.AreEqual([1I; 13I], NumbersDividers.GetDividers(13I))
        Assert.AreEqual([1I; 2I; 7I; 14I], NumbersDividers.GetDividers(14I))
        Assert.AreEqual([1I; 3I; 5I; 15I], NumbersDividers.GetDividers(15I))

    [<Test>]
    member public this.GetPrimeDividers() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(-1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(0) |> ignore) |> ignore
        Assert.AreEqual([], NumbersDividers.GetPrimeDividers(1))
        Assert.AreEqual([2], NumbersDividers.GetPrimeDividers(2))
        Assert.AreEqual([3], NumbersDividers.GetPrimeDividers(3))
        Assert.AreEqual([2], NumbersDividers.GetPrimeDividers(4))
        Assert.AreEqual([5], NumbersDividers.GetPrimeDividers(5))
        Assert.AreEqual([2; 3], NumbersDividers.GetPrimeDividers(6))
        Assert.AreEqual([7], NumbersDividers.GetPrimeDividers(7))
        Assert.AreEqual([2], NumbersDividers.GetPrimeDividers(8))
        Assert.AreEqual([3], NumbersDividers.GetPrimeDividers(9))
        Assert.AreEqual([2; 5], NumbersDividers.GetPrimeDividers(10))
        Assert.AreEqual([11], NumbersDividers.GetPrimeDividers(11))
        Assert.AreEqual([2; 3], NumbersDividers.GetPrimeDividers(12))
        Assert.AreEqual([13], NumbersDividers.GetPrimeDividers(13))
        Assert.AreEqual([2; 7], NumbersDividers.GetPrimeDividers(14))
        Assert.AreEqual([3; 5], NumbersDividers.GetPrimeDividers(15))
        Assert.AreEqual([2; 3; 5; 11; 13], NumbersDividers.GetPrimeDividers(2 * 2 * 3 * 3 * 3 * 5 * 5 * 11 * 13))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(-1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(0L) |> ignore) |> ignore
        Assert.AreEqual([], NumbersDividers.GetPrimeDividers(1L))
        Assert.AreEqual([2L], NumbersDividers.GetPrimeDividers(2L))
        Assert.AreEqual([3L], NumbersDividers.GetPrimeDividers(3L))
        Assert.AreEqual([2L], NumbersDividers.GetPrimeDividers(4L))
        Assert.AreEqual([5L], NumbersDividers.GetPrimeDividers(5L))
        Assert.AreEqual([2L; 3L], NumbersDividers.GetPrimeDividers(6L))
        Assert.AreEqual([7L], NumbersDividers.GetPrimeDividers(7L))
        Assert.AreEqual([2L], NumbersDividers.GetPrimeDividers(8L))
        Assert.AreEqual([3L], NumbersDividers.GetPrimeDividers(9L))
        Assert.AreEqual([2L; 5L], NumbersDividers.GetPrimeDividers(10L))
        Assert.AreEqual([11L], NumbersDividers.GetPrimeDividers(11L))
        Assert.AreEqual([2L; 3L], NumbersDividers.GetPrimeDividers(12L))
        Assert.AreEqual([13L], NumbersDividers.GetPrimeDividers(13L))
        Assert.AreEqual([2L; 7L], NumbersDividers.GetPrimeDividers(14L))
        Assert.AreEqual([3L; 5L], NumbersDividers.GetPrimeDividers(15L))
        Assert.AreEqual([2L; 3L; 5L; 11L; 13L], NumbersDividers.GetPrimeDividers(2L * 2L * 3L * 3L * 3L * 5L * 5L * 11L * 13L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(-1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDividers.GetPrimeDividers(0I) |> ignore) |> ignore
        Assert.AreEqual([], NumbersDividers.GetPrimeDividers(1I))
        Assert.AreEqual([2I], NumbersDividers.GetPrimeDividers(2I))
        Assert.AreEqual([3I], NumbersDividers.GetPrimeDividers(3I))
        Assert.AreEqual([2I], NumbersDividers.GetPrimeDividers(4I))
        Assert.AreEqual([5I], NumbersDividers.GetPrimeDividers(5I))
        Assert.AreEqual([2I; 3I], NumbersDividers.GetPrimeDividers(6I))
        Assert.AreEqual([7I], NumbersDividers.GetPrimeDividers(7I))
        Assert.AreEqual([2I], NumbersDividers.GetPrimeDividers(8I))
        Assert.AreEqual([3I], NumbersDividers.GetPrimeDividers(9I))
        Assert.AreEqual([2I; 5I], NumbersDividers.GetPrimeDividers(10I))
        Assert.AreEqual([11I], NumbersDividers.GetPrimeDividers(11I))
        Assert.AreEqual([2I; 3I], NumbersDividers.GetPrimeDividers(12I))
        Assert.AreEqual([13I], NumbersDividers.GetPrimeDividers(13I))
        Assert.AreEqual([2I; 7I], NumbersDividers.GetPrimeDividers(14I))
        Assert.AreEqual([3I; 5I], NumbersDividers.GetPrimeDividers(15I))
        Assert.AreEqual([2I; 3I; 5I; 11I; 13I], NumbersDividers.GetPrimeDividers(2I * 2I * 3I * 3I * 3I * 5I * 5I * 11I * 13I))