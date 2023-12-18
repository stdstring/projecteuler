namespace CommonLibTests

open CommonLib
open NUnit.Framework
open NUnit.Framework.Legacy
open System

[<TestFixture>]
type NumbersDigitsTests() =

    let maxint = Int32.MaxValue
    let maxint64 = Int64.MaxValue

    [<Test>]
    member public this.GetDigits() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1))
        ClassicAssert.AreEqual([5], NumbersDigits.GetDigits(5))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(10))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(11))
        ClassicAssert.AreEqual([2; 9], NumbersDigits.GetDigits(29))
        ClassicAssert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0], NumbersDigits.GetDigits(1234567890))
        ClassicAssert.AreEqual([2; 1; 4; 7; 4; 8; 3; 6; 4; 7], NumbersDigits.GetDigits(maxint))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1L) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0L))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1L))
        ClassicAssert.AreEqual([5], NumbersDigits.GetDigits(5L))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(10L))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(11L))
        ClassicAssert.AreEqual([2; 9], NumbersDigits.GetDigits(29L))
        ClassicAssert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0], NumbersDigits.GetDigits(1234567890L))
        ClassicAssert.AreEqual([2; 1; 4; 7; 4; 8; 3; 6; 4; 7], NumbersDigits.GetDigits(int64 maxint))
        ClassicAssert.AreEqual([9; 2; 2; 3; 3; 7; 2; 0; 3; 6; 8; 5; 4; 7; 7; 5; 8; 0; 7], NumbersDigits.GetDigits(maxint64))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1I) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0I))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1I))
        ClassicAssert.AreEqual([5], NumbersDigits.GetDigits(5I))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(10I))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(11I))
        ClassicAssert.AreEqual([2; 9], NumbersDigits.GetDigits(29I))
        ClassicAssert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0], NumbersDigits.GetDigits(1234567890I))
        ClassicAssert.AreEqual([2; 1; 4; 7; 4; 8; 3; 6; 4; 7], NumbersDigits.GetDigits(bigint maxint))
        ClassicAssert.AreEqual([9; 2; 2; 3; 3; 7; 2; 0; 3; 6; 8; 5; 4; 7; 7; 5; 8; 0; 7], NumbersDigits.GetDigits(bigint maxint64))
        ClassicAssert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 0], NumbersDigits.GetDigits(12345678901234567890I))

    [<Test>]
    member public this.GetDigitsWithRadix() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(1, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(1, 0) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> NumbersDigits.GetDigits(1, 11) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1, 10) |> ignore) |> ignore
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(1L, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(1L, 0) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> NumbersDigits.GetDigits(1L, 11) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1L, 10) |> ignore) |> ignore
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(1I, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(1I, 0) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> NumbersDigits.GetDigits(1I, 11) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1I, 10) |> ignore) |> ignore

    [<Test>]
    member public this.GetDigitsWithRadix2() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1, 2) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0, 2))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1, 2))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(2, 2))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(3, 2))
        ClassicAssert.AreEqual([1; 0; 0], NumbersDigits.GetDigits(4, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetDigits(5, 2))
        ClassicAssert.AreEqual([1; 1; 0], NumbersDigits.GetDigits(6, 2))
        ClassicAssert.AreEqual([1; 1; 1], NumbersDigits.GetDigits(7, 2))
        ClassicAssert.AreEqual([1; 0; 0; 0], NumbersDigits.GetDigits(8, 2))
        ClassicAssert.AreEqual([1; 0; 0; 1], NumbersDigits.GetDigits(9, 2))
        ClassicAssert.AreEqual([1; 0; 1; 0], NumbersDigits.GetDigits(10, 2))
        ClassicAssert.AreEqual([1; 0; 1; 1], NumbersDigits.GetDigits(11, 2))
        ClassicAssert.AreEqual([1; 1; 0; 0], NumbersDigits.GetDigits(12, 2))
        ClassicAssert.AreEqual([1; 1; 0; 1], NumbersDigits.GetDigits(13, 2))
        ClassicAssert.AreEqual([1; 1; 1; 0], NumbersDigits.GetDigits(14, 2))
        ClassicAssert.AreEqual([1; 1; 1; 1], NumbersDigits.GetDigits(15, 2))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1L, 2) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0L, 2))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1L, 2))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(2L, 2))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(3L, 2))
        ClassicAssert.AreEqual([1; 0; 0], NumbersDigits.GetDigits(4L, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetDigits(5L, 2))
        ClassicAssert.AreEqual([1; 1; 0], NumbersDigits.GetDigits(6L, 2))
        ClassicAssert.AreEqual([1; 1; 1], NumbersDigits.GetDigits(7L, 2))
        ClassicAssert.AreEqual([1; 0; 0; 0], NumbersDigits.GetDigits(8L, 2))
        ClassicAssert.AreEqual([1; 0; 0; 1], NumbersDigits.GetDigits(9L, 2))
        ClassicAssert.AreEqual([1; 0; 1; 0], NumbersDigits.GetDigits(10L, 2))
        ClassicAssert.AreEqual([1; 0; 1; 1], NumbersDigits.GetDigits(11L, 2))
        ClassicAssert.AreEqual([1; 1; 0; 0], NumbersDigits.GetDigits(12L, 2))
        ClassicAssert.AreEqual([1; 1; 0; 1], NumbersDigits.GetDigits(13L, 2))
        ClassicAssert.AreEqual([1; 1; 1; 0], NumbersDigits.GetDigits(14L, 2))
        ClassicAssert.AreEqual([1; 1; 1; 1], NumbersDigits.GetDigits(15L, 2))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1I, 2) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0I, 2))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1I, 2))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(2I, 2))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(3I, 2))
        ClassicAssert.AreEqual([1; 0; 0], NumbersDigits.GetDigits(4I, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetDigits(5I, 2))
        ClassicAssert.AreEqual([1; 1; 0], NumbersDigits.GetDigits(6I, 2))
        ClassicAssert.AreEqual([1; 1; 1], NumbersDigits.GetDigits(7I, 2))
        ClassicAssert.AreEqual([1; 0; 0; 0], NumbersDigits.GetDigits(8I, 2))
        ClassicAssert.AreEqual([1; 0; 0; 1], NumbersDigits.GetDigits(9I, 2))
        ClassicAssert.AreEqual([1; 0; 1; 0], NumbersDigits.GetDigits(10I, 2))
        ClassicAssert.AreEqual([1; 0; 1; 1], NumbersDigits.GetDigits(11I, 2))
        ClassicAssert.AreEqual([1; 1; 0; 0], NumbersDigits.GetDigits(12I, 2))
        ClassicAssert.AreEqual([1; 1; 0; 1], NumbersDigits.GetDigits(13I, 2))
        ClassicAssert.AreEqual([1; 1; 1; 0], NumbersDigits.GetDigits(14I, 2))
        ClassicAssert.AreEqual([1; 1; 1; 1], NumbersDigits.GetDigits(15I, 2))

    [<Test>]
    member public this.GetDigitsWithRadix3() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1, 3) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0, 3))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1, 3))
        ClassicAssert.AreEqual([2], NumbersDigits.GetDigits(2, 3))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(3, 3))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(4, 3))
        ClassicAssert.AreEqual([1; 2], NumbersDigits.GetDigits(5, 3))
        ClassicAssert.AreEqual([2; 0], NumbersDigits.GetDigits(6, 3))
        ClassicAssert.AreEqual([2; 1], NumbersDigits.GetDigits(7, 3))
        ClassicAssert.AreEqual([2; 2], NumbersDigits.GetDigits(8, 3))
        ClassicAssert.AreEqual([1; 0; 0], NumbersDigits.GetDigits(9, 3))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetDigits(10, 3))
        ClassicAssert.AreEqual([1; 0; 2], NumbersDigits.GetDigits(11, 3))
        ClassicAssert.AreEqual([1; 1; 0], NumbersDigits.GetDigits(12, 3))
        ClassicAssert.AreEqual([1; 1; 1], NumbersDigits.GetDigits(13, 3))
        ClassicAssert.AreEqual([1; 1; 2], NumbersDigits.GetDigits(14, 3))
        ClassicAssert.AreEqual([1; 2; 0], NumbersDigits.GetDigits(15, 3))
        ClassicAssert.AreEqual([1; 2; 1], NumbersDigits.GetDigits(16, 3))
        ClassicAssert.AreEqual([1; 2; 2], NumbersDigits.GetDigits(17, 3))
        ClassicAssert.AreEqual([2; 0; 0], NumbersDigits.GetDigits(18, 3))
        ClassicAssert.AreEqual([2; 0; 1], NumbersDigits.GetDigits(19, 3))
        ClassicAssert.AreEqual([2; 0; 2], NumbersDigits.GetDigits(20, 3))
        ClassicAssert.AreEqual([2; 1; 0], NumbersDigits.GetDigits(21, 3))
        ClassicAssert.AreEqual([2; 1; 1], NumbersDigits.GetDigits(22, 3))
        ClassicAssert.AreEqual([2; 1; 2], NumbersDigits.GetDigits(23, 3))
        ClassicAssert.AreEqual([2; 2; 0], NumbersDigits.GetDigits(24, 3))
        ClassicAssert.AreEqual([2; 2; 1], NumbersDigits.GetDigits(25, 3))
        ClassicAssert.AreEqual([2; 2; 2], NumbersDigits.GetDigits(26, 3))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1L, 3) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0L, 3))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1L, 3))
        ClassicAssert.AreEqual([2], NumbersDigits.GetDigits(2L, 3))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(3L, 3))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(4L, 3))
        ClassicAssert.AreEqual([1; 2], NumbersDigits.GetDigits(5L, 3))
        ClassicAssert.AreEqual([2; 0], NumbersDigits.GetDigits(6L, 3))
        ClassicAssert.AreEqual([2; 1], NumbersDigits.GetDigits(7L, 3))
        ClassicAssert.AreEqual([2; 2], NumbersDigits.GetDigits(8L, 3))
        ClassicAssert.AreEqual([1; 0; 0], NumbersDigits.GetDigits(9L, 3))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetDigits(10L, 3))
        ClassicAssert.AreEqual([1; 0; 2], NumbersDigits.GetDigits(11L, 3))
        ClassicAssert.AreEqual([1; 1; 0], NumbersDigits.GetDigits(12L, 3))
        ClassicAssert.AreEqual([1; 1; 1], NumbersDigits.GetDigits(13L, 3))
        ClassicAssert.AreEqual([1; 1; 2], NumbersDigits.GetDigits(14L, 3))
        ClassicAssert.AreEqual([1; 2; 0], NumbersDigits.GetDigits(15L, 3))
        ClassicAssert.AreEqual([1; 2; 1], NumbersDigits.GetDigits(16L, 3))
        ClassicAssert.AreEqual([1; 2; 2], NumbersDigits.GetDigits(17L, 3))
        ClassicAssert.AreEqual([2; 0; 0], NumbersDigits.GetDigits(18L, 3))
        ClassicAssert.AreEqual([2; 0; 1], NumbersDigits.GetDigits(19L, 3))
        ClassicAssert.AreEqual([2; 0; 2], NumbersDigits.GetDigits(20L, 3))
        ClassicAssert.AreEqual([2; 1; 0], NumbersDigits.GetDigits(21L, 3))
        ClassicAssert.AreEqual([2; 1; 1], NumbersDigits.GetDigits(22L, 3))
        ClassicAssert.AreEqual([2; 1; 2], NumbersDigits.GetDigits(23L, 3))
        ClassicAssert.AreEqual([2; 2; 0], NumbersDigits.GetDigits(24L, 3))
        ClassicAssert.AreEqual([2; 2; 1], NumbersDigits.GetDigits(25L, 3))
        ClassicAssert.AreEqual([2; 2; 2], NumbersDigits.GetDigits(26L, 3))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetDigits(-1I, 3) |> ignore) |> ignore
        ClassicAssert.AreEqual([0], NumbersDigits.GetDigits(0I, 3))
        ClassicAssert.AreEqual([1], NumbersDigits.GetDigits(1I, 3))
        ClassicAssert.AreEqual([2], NumbersDigits.GetDigits(2I, 3))
        ClassicAssert.AreEqual([1; 0], NumbersDigits.GetDigits(3I, 3))
        ClassicAssert.AreEqual([1; 1], NumbersDigits.GetDigits(4I, 3))
        ClassicAssert.AreEqual([1; 2], NumbersDigits.GetDigits(5I, 3))
        ClassicAssert.AreEqual([2; 0], NumbersDigits.GetDigits(6I, 3))
        ClassicAssert.AreEqual([2; 1], NumbersDigits.GetDigits(7I, 3))
        ClassicAssert.AreEqual([2; 2], NumbersDigits.GetDigits(8I, 3))
        ClassicAssert.AreEqual([1; 0; 0], NumbersDigits.GetDigits(9I, 3))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetDigits(10I, 3))
        ClassicAssert.AreEqual([1; 0; 2], NumbersDigits.GetDigits(11I, 3))
        ClassicAssert.AreEqual([1; 1; 0], NumbersDigits.GetDigits(12I, 3))
        ClassicAssert.AreEqual([1; 1; 1], NumbersDigits.GetDigits(13I, 3))
        ClassicAssert.AreEqual([1; 1; 2], NumbersDigits.GetDigits(14I, 3))
        ClassicAssert.AreEqual([1; 2; 0], NumbersDigits.GetDigits(15I, 3))
        ClassicAssert.AreEqual([1; 2; 1], NumbersDigits.GetDigits(16I, 3))
        ClassicAssert.AreEqual([1; 2; 2], NumbersDigits.GetDigits(17I, 3))
        ClassicAssert.AreEqual([2; 0; 0], NumbersDigits.GetDigits(18I, 3))
        ClassicAssert.AreEqual([2; 0; 1], NumbersDigits.GetDigits(19I, 3))
        ClassicAssert.AreEqual([2; 0; 2], NumbersDigits.GetDigits(20I, 3))
        ClassicAssert.AreEqual([2; 1; 0], NumbersDigits.GetDigits(21I, 3))
        ClassicAssert.AreEqual([2; 1; 1], NumbersDigits.GetDigits(22I, 3))
        ClassicAssert.AreEqual([2; 1; 2], NumbersDigits.GetDigits(23I, 3))
        ClassicAssert.AreEqual([2; 2; 0], NumbersDigits.GetDigits(24I, 3))
        ClassicAssert.AreEqual([2; 2; 1], NumbersDigits.GetDigits(25I, 3))
        ClassicAssert.AreEqual([2; 2; 2], NumbersDigits.GetDigits(26I, 3))

    [<Test>]
    member public this.GetFixedSizeDigits() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(-1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1, -1) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], NumbersDigits.GetFixedSizeDigits(1, 1))
        ClassicAssert.AreEqual([0; 1], NumbersDigits.GetFixedSizeDigits(1, 2))
        ClassicAssert.AreEqual([0; 0; 1], NumbersDigits.GetFixedSizeDigits(1, 3))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123, 1))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123, 2))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123, 3))
        ClassicAssert.AreEqual([0; 1; 2; 3], NumbersDigits.GetFixedSizeDigits(123, 4))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(-1L, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1L, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1L, -1) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], NumbersDigits.GetFixedSizeDigits(1L, 1))
        ClassicAssert.AreEqual([0; 1], NumbersDigits.GetFixedSizeDigits(1L, 2))
        ClassicAssert.AreEqual([0; 0; 1], NumbersDigits.GetFixedSizeDigits(1L, 3))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123L, 1))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123L, 2))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123L, 3))
        ClassicAssert.AreEqual([0; 1; 2; 3], NumbersDigits.GetFixedSizeDigits(123L, 4))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(-1I, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1I, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1I, -1) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], NumbersDigits.GetFixedSizeDigits(1I, 1))
        ClassicAssert.AreEqual([0; 1], NumbersDigits.GetFixedSizeDigits(1I, 2))
        ClassicAssert.AreEqual([0; 0; 1], NumbersDigits.GetFixedSizeDigits(1I, 3))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123I, 1))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123I, 2))
        ClassicAssert.AreEqual([1; 2; 3], NumbersDigits.GetFixedSizeDigits(123I, 3))
        ClassicAssert.AreEqual([0; 1; 2; 3], NumbersDigits.GetFixedSizeDigits(123I, 4))

    [<Test>]
    member public this.GetFixedSizeDigitsWithRadix() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(-1, 10, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1, -1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1, 0, 1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> NumbersDigits.GetFixedSizeDigits(1, 11, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1, 2, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1, 2, 0) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], NumbersDigits.GetFixedSizeDigits(1, 2, 1))
        ClassicAssert.AreEqual([0; 1], NumbersDigits.GetFixedSizeDigits(1, 2, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5, 2, 1))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5, 2, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5, 2, 3))
        ClassicAssert.AreEqual([0; 1; 0; 1], NumbersDigits.GetFixedSizeDigits(5, 2, 4))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(-1L, 10, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1L, -1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1L, 0, 1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> NumbersDigits.GetFixedSizeDigits(1L, 11, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1L, 2, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1L, 2, 0) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], NumbersDigits.GetFixedSizeDigits(1L, 2, 1))
        ClassicAssert.AreEqual([0; 1], NumbersDigits.GetFixedSizeDigits(1L, 2, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5L, 2, 1))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5L, 2, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5L, 2, 3))
        ClassicAssert.AreEqual([0; 1; 0; 1], NumbersDigits.GetFixedSizeDigits(5L, 2, 4))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(-1I, 10, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1I, -1, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1I, 0, 1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> NumbersDigits.GetFixedSizeDigits(1I, 11, 1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1I, 2, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetFixedSizeDigits(1I, 2, 0) |> ignore) |> ignore
        ClassicAssert.AreEqual([1], NumbersDigits.GetFixedSizeDigits(1I, 2, 1))
        ClassicAssert.AreEqual([0; 1], NumbersDigits.GetFixedSizeDigits(1I, 2, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5I, 2, 1))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5I, 2, 2))
        ClassicAssert.AreEqual([1; 0; 1], NumbersDigits.GetFixedSizeDigits(5I, 2, 3))
        ClassicAssert.AreEqual([0; 1; 0; 1], NumbersDigits.GetFixedSizeDigits(5I, 2, 4))

    [<Test>]
    member public this.GetNumber() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; -1]) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; 11]) |> ignore) |> ignore
        ClassicAssert.AreEqual(0I, NumbersDigits.GetNumber([0]))
        ClassicAssert.AreEqual(1I, NumbersDigits.GetNumber([1]))
        ClassicAssert.AreEqual(5I, NumbersDigits.GetNumber([5]))
        ClassicAssert.AreEqual(10I, NumbersDigits.GetNumber([1; 0]))
        ClassicAssert.AreEqual(11I, NumbersDigits.GetNumber([1; 1]))
        ClassicAssert.AreEqual(29I, NumbersDigits.GetNumber([2; 9]))
        ClassicAssert.AreEqual(1234567890I, NumbersDigits.GetNumber([1; 2; 3; 4; 5; 6; 7; 8; 9; 0]))
        ClassicAssert.AreEqual(bigint maxint, NumbersDigits.GetNumber([2; 1; 4; 7; 4; 8; 3; 6; 4; 7]))
        ClassicAssert.AreEqual(bigint maxint64, NumbersDigits.GetNumber([9; 2; 2; 3; 3; 7; 2; 0; 3; 6; 8; 5; 4; 7; 7; 5; 8; 0; 7]))
        ClassicAssert.AreEqual(12345678901234567890I, NumbersDigits.GetNumber([1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 0]))

    [<Test>]
    member public this.GetNumberWithRadix() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; -1], 2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; 3], 2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; 2], -1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> NumbersDigits.GetNumber([0; 1; 2], 11) |> ignore) |> ignore

    [<Test>]
    member public this.GetNumberWithRadix2() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; -1], 2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; 2], 2) |> ignore) |> ignore
        ClassicAssert.AreEqual(0I, NumbersDigits.GetNumber([0], 2))
        ClassicAssert.AreEqual(1I, NumbersDigits.GetNumber([1], 2))
        ClassicAssert.AreEqual(2I, NumbersDigits.GetNumber([1; 0], 2))
        ClassicAssert.AreEqual(3I, NumbersDigits.GetNumber([1; 1], 2))
        ClassicAssert.AreEqual(4I, NumbersDigits.GetNumber([1; 0; 0], 2))
        ClassicAssert.AreEqual(5I, NumbersDigits.GetNumber([1; 0; 1], 2))
        ClassicAssert.AreEqual(6I, NumbersDigits.GetNumber([1; 1; 0], 2))
        ClassicAssert.AreEqual(7I, NumbersDigits.GetNumber([1; 1; 1], 2))
        ClassicAssert.AreEqual(8I, NumbersDigits.GetNumber([1; 0; 0; 0], 2))
        ClassicAssert.AreEqual(9I, NumbersDigits.GetNumber([1; 0; 0; 1], 2))
        ClassicAssert.AreEqual(10I, NumbersDigits.GetNumber([1; 0; 1; 0], 2))
        ClassicAssert.AreEqual(11I, NumbersDigits.GetNumber([1; 0; 1; 1], 2))
        ClassicAssert.AreEqual(12I, NumbersDigits.GetNumber([1; 1; 0; 0], 2))
        ClassicAssert.AreEqual(13I, NumbersDigits.GetNumber([1; 1; 0; 1], 2))
        ClassicAssert.AreEqual(14I, NumbersDigits.GetNumber([1; 1; 1; 0], 2))
        ClassicAssert.AreEqual(15I, NumbersDigits.GetNumber([1; 1; 1; 1], 2))

    [<Test>]
    member public this.GetNumberWithRadix3() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; -1], 3) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.GetNumber([0; 1; 3], 3) |> ignore) |> ignore
        ClassicAssert.AreEqual(0I, NumbersDigits.GetNumber([0], 3))
        ClassicAssert.AreEqual(1I, NumbersDigits.GetNumber([1], 3))
        ClassicAssert.AreEqual(2I, NumbersDigits.GetNumber([2], 3))
        ClassicAssert.AreEqual(3I, NumbersDigits.GetNumber([1; 0], 3))
        ClassicAssert.AreEqual(4I, NumbersDigits.GetNumber([1; 1], 3))
        ClassicAssert.AreEqual(5I, NumbersDigits.GetNumber([1; 2], 3))
        ClassicAssert.AreEqual(6I, NumbersDigits.GetNumber([2; 0], 3))
        ClassicAssert.AreEqual(7I, NumbersDigits.GetNumber([2; 1], 3))
        ClassicAssert.AreEqual(8I, NumbersDigits.GetNumber([2; 2], 3))
        ClassicAssert.AreEqual(9I, NumbersDigits.GetNumber([1; 0; 0], 3))
        ClassicAssert.AreEqual(10I, NumbersDigits.GetNumber([1; 0; 1], 3))
        ClassicAssert.AreEqual(11I, NumbersDigits.GetNumber([1; 0; 2], 3))
        ClassicAssert.AreEqual(12I, NumbersDigits.GetNumber([1; 1; 0], 3))
        ClassicAssert.AreEqual(13I, NumbersDigits.GetNumber([1; 1; 1], 3))
        ClassicAssert.AreEqual(14I, NumbersDigits.GetNumber([1; 1; 2], 3))
        ClassicAssert.AreEqual(15I, NumbersDigits.GetNumber([1; 2; 0], 3))
        ClassicAssert.AreEqual(16I, NumbersDigits.GetNumber([1; 2; 1], 3))
        ClassicAssert.AreEqual(17I, NumbersDigits.GetNumber([1; 2; 2], 3))
        ClassicAssert.AreEqual(18I, NumbersDigits.GetNumber([2; 0; 0], 3))
        ClassicAssert.AreEqual(19I, NumbersDigits.GetNumber([2; 0; 1], 3))
        ClassicAssert.AreEqual(20I, NumbersDigits.GetNumber([2; 0; 2], 3))
        ClassicAssert.AreEqual(21I, NumbersDigits.GetNumber([2; 1; 0], 3))
        ClassicAssert.AreEqual(22I, NumbersDigits.GetNumber([2; 1; 1], 3))
        ClassicAssert.AreEqual(23I, NumbersDigits.GetNumber([2; 1; 2], 3))
        ClassicAssert.AreEqual(24I, NumbersDigits.GetNumber([2; 2; 0], 3))
        ClassicAssert.AreEqual(25I, NumbersDigits.GetNumber([2; 2; 1], 3))
        ClassicAssert.AreEqual(26I, NumbersDigits.GetNumber([2; 2; 2], 3))

    [<Test>]
    member public this.ReverseNumber() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.ReverseNumber(-1) |> ignore) |> ignore
        ClassicAssert.AreEqual(0, NumbersDigits.ReverseNumber(0))
        ClassicAssert.AreEqual(1, NumbersDigits.ReverseNumber(1))
        ClassicAssert.AreEqual(1, NumbersDigits.ReverseNumber(01))
        ClassicAssert.AreEqual(1, NumbersDigits.ReverseNumber(10))
        ClassicAssert.AreEqual(11, NumbersDigits.ReverseNumber(11))
        ClassicAssert.AreEqual(31, NumbersDigits.ReverseNumber(13))
        ClassicAssert.AreEqual(92, NumbersDigits.ReverseNumber(29))
        ClassicAssert.AreEqual(1, NumbersDigits.ReverseNumber(100))
        ClassicAssert.AreEqual(21, NumbersDigits.ReverseNumber(120))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.ReverseNumber(-1L) |> ignore) |> ignore
        ClassicAssert.AreEqual(0L, NumbersDigits.ReverseNumber(0L))
        ClassicAssert.AreEqual(1L, NumbersDigits.ReverseNumber(1L))
        ClassicAssert.AreEqual(1L, NumbersDigits.ReverseNumber(01L))
        ClassicAssert.AreEqual(1L, NumbersDigits.ReverseNumber(10L))
        ClassicAssert.AreEqual(11L, NumbersDigits.ReverseNumber(11L))
        ClassicAssert.AreEqual(31L, NumbersDigits.ReverseNumber(13L))
        ClassicAssert.AreEqual(92L, NumbersDigits.ReverseNumber(29L))
        ClassicAssert.AreEqual(1L, NumbersDigits.ReverseNumber(100L))
        ClassicAssert.AreEqual(21L, NumbersDigits.ReverseNumber(120L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> NumbersDigits.ReverseNumber(-1I) |> ignore) |> ignore
        ClassicAssert.AreEqual(0I, NumbersDigits.ReverseNumber(0I))
        ClassicAssert.AreEqual(1I, NumbersDigits.ReverseNumber(1I))
        ClassicAssert.AreEqual(1I, NumbersDigits.ReverseNumber(01I))
        ClassicAssert.AreEqual(1I, NumbersDigits.ReverseNumber(10I))
        ClassicAssert.AreEqual(11I, NumbersDigits.ReverseNumber(11I))
        ClassicAssert.AreEqual(31I, NumbersDigits.ReverseNumber(13I))
        ClassicAssert.AreEqual(92I, NumbersDigits.ReverseNumber(29I))
        ClassicAssert.AreEqual(1I, NumbersDigits.ReverseNumber(100I))
        ClassicAssert.AreEqual(21I, NumbersDigits.ReverseNumber(120I))
