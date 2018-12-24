namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib

[<TestFixture>]
type NumbersTests() =

    let maxint = Int32.MaxValue
    let maxint64 = Int64.MaxValue

    [<Test>]
    member public this.GetDigits() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0))
        Assert.AreEqual([1], Numbers.GetDigits(1))
        Assert.AreEqual([5], Numbers.GetDigits(5))
        Assert.AreEqual([1; 0], Numbers.GetDigits(10))
        Assert.AreEqual([1; 1], Numbers.GetDigits(11))
        Assert.AreEqual([2; 9], Numbers.GetDigits(29))
        Assert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0], Numbers.GetDigits(1234567890))
        Assert.AreEqual([2; 1; 4; 7; 4; 8; 3; 6; 4; 7], Numbers.GetDigits(maxint))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1L) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0L))
        Assert.AreEqual([1], Numbers.GetDigits(1L))
        Assert.AreEqual([5], Numbers.GetDigits(5L))
        Assert.AreEqual([1; 0], Numbers.GetDigits(10L))
        Assert.AreEqual([1; 1], Numbers.GetDigits(11L))
        Assert.AreEqual([2; 9], Numbers.GetDigits(29L))
        Assert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0], Numbers.GetDigits(1234567890L))
        Assert.AreEqual([2; 1; 4; 7; 4; 8; 3; 6; 4; 7], Numbers.GetDigits(int64 maxint))
        Assert.AreEqual([9; 2; 2; 3; 3; 7; 2; 0; 3; 6; 8; 5; 4; 7; 7; 5; 8; 0; 7], Numbers.GetDigits(maxint64))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1I) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0I))
        Assert.AreEqual([1], Numbers.GetDigits(1I))
        Assert.AreEqual([5], Numbers.GetDigits(5I))
        Assert.AreEqual([1; 0], Numbers.GetDigits(10I))
        Assert.AreEqual([1; 1], Numbers.GetDigits(11I))
        Assert.AreEqual([2; 9], Numbers.GetDigits(29I))
        Assert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0], Numbers.GetDigits(1234567890I))
        Assert.AreEqual([2; 1; 4; 7; 4; 8; 3; 6; 4; 7], Numbers.GetDigits(bigint maxint))
        Assert.AreEqual([9; 2; 2; 3; 3; 7; 2; 0; 3; 6; 8; 5; 4; 7; 7; 5; 8; 0; 7], Numbers.GetDigits(bigint maxint64))
        Assert.AreEqual([1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 0], Numbers.GetDigits(12345678901234567890I))

    [<Test>]
    member public this.GetDigitsWithRadix() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(1, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(1, 0) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> Numbers.GetDigits(1, 11) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1, 10) |> ignore) |> ignore
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(1L, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(1L, 0) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> Numbers.GetDigits(1L, 11) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1L, 10) |> ignore) |> ignore
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(1I, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(1I, 0) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> Numbers.GetDigits(1I, 11) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1I, 10) |> ignore) |> ignore

    [<Test>]
    member public this.GetDigitsWithRadix2() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1, 2) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0, 2))
        Assert.AreEqual([1], Numbers.GetDigits(1, 2))
        Assert.AreEqual([1; 0], Numbers.GetDigits(2, 2))
        Assert.AreEqual([1; 1], Numbers.GetDigits(3, 2))
        Assert.AreEqual([1; 0; 0], Numbers.GetDigits(4, 2))
        Assert.AreEqual([1; 0; 1], Numbers.GetDigits(5, 2))
        Assert.AreEqual([1; 1; 0], Numbers.GetDigits(6, 2))
        Assert.AreEqual([1; 1; 1], Numbers.GetDigits(7, 2))
        Assert.AreEqual([1; 0; 0; 0], Numbers.GetDigits(8, 2))
        Assert.AreEqual([1; 0; 0; 1], Numbers.GetDigits(9, 2))
        Assert.AreEqual([1; 0; 1; 0], Numbers.GetDigits(10, 2))
        Assert.AreEqual([1; 0; 1; 1], Numbers.GetDigits(11, 2))
        Assert.AreEqual([1; 1; 0; 0], Numbers.GetDigits(12, 2))
        Assert.AreEqual([1; 1; 0; 1], Numbers.GetDigits(13, 2))
        Assert.AreEqual([1; 1; 1; 0], Numbers.GetDigits(14, 2))
        Assert.AreEqual([1; 1; 1; 1], Numbers.GetDigits(15, 2))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1L, 2) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0L, 2))
        Assert.AreEqual([1], Numbers.GetDigits(1L, 2))
        Assert.AreEqual([1; 0], Numbers.GetDigits(2L, 2))
        Assert.AreEqual([1; 1], Numbers.GetDigits(3L, 2))
        Assert.AreEqual([1; 0; 0], Numbers.GetDigits(4L, 2))
        Assert.AreEqual([1; 0; 1], Numbers.GetDigits(5L, 2))
        Assert.AreEqual([1; 1; 0], Numbers.GetDigits(6L, 2))
        Assert.AreEqual([1; 1; 1], Numbers.GetDigits(7L, 2))
        Assert.AreEqual([1; 0; 0; 0], Numbers.GetDigits(8L, 2))
        Assert.AreEqual([1; 0; 0; 1], Numbers.GetDigits(9L, 2))
        Assert.AreEqual([1; 0; 1; 0], Numbers.GetDigits(10L, 2))
        Assert.AreEqual([1; 0; 1; 1], Numbers.GetDigits(11L, 2))
        Assert.AreEqual([1; 1; 0; 0], Numbers.GetDigits(12L, 2))
        Assert.AreEqual([1; 1; 0; 1], Numbers.GetDigits(13L, 2))
        Assert.AreEqual([1; 1; 1; 0], Numbers.GetDigits(14L, 2))
        Assert.AreEqual([1; 1; 1; 1], Numbers.GetDigits(15L, 2))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1I, 2) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0I, 2))
        Assert.AreEqual([1], Numbers.GetDigits(1I, 2))
        Assert.AreEqual([1; 0], Numbers.GetDigits(2I, 2))
        Assert.AreEqual([1; 1], Numbers.GetDigits(3I, 2))
        Assert.AreEqual([1; 0; 0], Numbers.GetDigits(4I, 2))
        Assert.AreEqual([1; 0; 1], Numbers.GetDigits(5I, 2))
        Assert.AreEqual([1; 1; 0], Numbers.GetDigits(6I, 2))
        Assert.AreEqual([1; 1; 1], Numbers.GetDigits(7I, 2))
        Assert.AreEqual([1; 0; 0; 0], Numbers.GetDigits(8I, 2))
        Assert.AreEqual([1; 0; 0; 1], Numbers.GetDigits(9I, 2))
        Assert.AreEqual([1; 0; 1; 0], Numbers.GetDigits(10I, 2))
        Assert.AreEqual([1; 0; 1; 1], Numbers.GetDigits(11I, 2))
        Assert.AreEqual([1; 1; 0; 0], Numbers.GetDigits(12I, 2))
        Assert.AreEqual([1; 1; 0; 1], Numbers.GetDigits(13I, 2))
        Assert.AreEqual([1; 1; 1; 0], Numbers.GetDigits(14I, 2))
        Assert.AreEqual([1; 1; 1; 1], Numbers.GetDigits(15I, 2))

    [<Test>]
    member public this.GetDigitsWithRadix3() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1, 3) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0, 3))
        Assert.AreEqual([1], Numbers.GetDigits(1, 3))
        Assert.AreEqual([2], Numbers.GetDigits(2, 3))
        Assert.AreEqual([1; 0], Numbers.GetDigits(3, 3))
        Assert.AreEqual([1; 1], Numbers.GetDigits(4, 3))
        Assert.AreEqual([1; 2], Numbers.GetDigits(5, 3))
        Assert.AreEqual([2; 0], Numbers.GetDigits(6, 3))
        Assert.AreEqual([2; 1], Numbers.GetDigits(7, 3))
        Assert.AreEqual([2; 2], Numbers.GetDigits(8, 3))
        Assert.AreEqual([1; 0; 0], Numbers.GetDigits(9, 3))
        Assert.AreEqual([1; 0; 1], Numbers.GetDigits(10, 3))
        Assert.AreEqual([1; 0; 2], Numbers.GetDigits(11, 3))
        Assert.AreEqual([1; 1; 0], Numbers.GetDigits(12, 3))
        Assert.AreEqual([1; 1; 1], Numbers.GetDigits(13, 3))
        Assert.AreEqual([1; 1; 2], Numbers.GetDigits(14, 3))
        Assert.AreEqual([1; 2; 0], Numbers.GetDigits(15, 3))
        Assert.AreEqual([1; 2; 1], Numbers.GetDigits(16, 3))
        Assert.AreEqual([1; 2; 2], Numbers.GetDigits(17, 3))
        Assert.AreEqual([2; 0; 0], Numbers.GetDigits(18, 3))
        Assert.AreEqual([2; 0; 1], Numbers.GetDigits(19, 3))
        Assert.AreEqual([2; 0; 2], Numbers.GetDigits(20, 3))
        Assert.AreEqual([2; 1; 0], Numbers.GetDigits(21, 3))
        Assert.AreEqual([2; 1; 1], Numbers.GetDigits(22, 3))
        Assert.AreEqual([2; 1; 2], Numbers.GetDigits(23, 3))
        Assert.AreEqual([2; 2; 0], Numbers.GetDigits(24, 3))
        Assert.AreEqual([2; 2; 1], Numbers.GetDigits(25, 3))
        Assert.AreEqual([2; 2; 2], Numbers.GetDigits(26, 3))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1L, 3) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0L, 3))
        Assert.AreEqual([1], Numbers.GetDigits(1L, 3))
        Assert.AreEqual([2], Numbers.GetDigits(2L, 3))
        Assert.AreEqual([1; 0], Numbers.GetDigits(3L, 3))
        Assert.AreEqual([1; 1], Numbers.GetDigits(4L, 3))
        Assert.AreEqual([1; 2], Numbers.GetDigits(5L, 3))
        Assert.AreEqual([2; 0], Numbers.GetDigits(6L, 3))
        Assert.AreEqual([2; 1], Numbers.GetDigits(7L, 3))
        Assert.AreEqual([2; 2], Numbers.GetDigits(8L, 3))
        Assert.AreEqual([1; 0; 0], Numbers.GetDigits(9L, 3))
        Assert.AreEqual([1; 0; 1], Numbers.GetDigits(10L, 3))
        Assert.AreEqual([1; 0; 2], Numbers.GetDigits(11L, 3))
        Assert.AreEqual([1; 1; 0], Numbers.GetDigits(12L, 3))
        Assert.AreEqual([1; 1; 1], Numbers.GetDigits(13L, 3))
        Assert.AreEqual([1; 1; 2], Numbers.GetDigits(14L, 3))
        Assert.AreEqual([1; 2; 0], Numbers.GetDigits(15L, 3))
        Assert.AreEqual([1; 2; 1], Numbers.GetDigits(16L, 3))
        Assert.AreEqual([1; 2; 2], Numbers.GetDigits(17L, 3))
        Assert.AreEqual([2; 0; 0], Numbers.GetDigits(18L, 3))
        Assert.AreEqual([2; 0; 1], Numbers.GetDigits(19L, 3))
        Assert.AreEqual([2; 0; 2], Numbers.GetDigits(20L, 3))
        Assert.AreEqual([2; 1; 0], Numbers.GetDigits(21L, 3))
        Assert.AreEqual([2; 1; 1], Numbers.GetDigits(22L, 3))
        Assert.AreEqual([2; 1; 2], Numbers.GetDigits(23L, 3))
        Assert.AreEqual([2; 2; 0], Numbers.GetDigits(24L, 3))
        Assert.AreEqual([2; 2; 1], Numbers.GetDigits(25L, 3))
        Assert.AreEqual([2; 2; 2], Numbers.GetDigits(26L, 3))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetDigits(-1I, 3) |> ignore) |> ignore
        Assert.AreEqual([0], Numbers.GetDigits(0I, 3))
        Assert.AreEqual([1], Numbers.GetDigits(1I, 3))
        Assert.AreEqual([2], Numbers.GetDigits(2I, 3))
        Assert.AreEqual([1; 0], Numbers.GetDigits(3I, 3))
        Assert.AreEqual([1; 1], Numbers.GetDigits(4I, 3))
        Assert.AreEqual([1; 2], Numbers.GetDigits(5I, 3))
        Assert.AreEqual([2; 0], Numbers.GetDigits(6I, 3))
        Assert.AreEqual([2; 1], Numbers.GetDigits(7I, 3))
        Assert.AreEqual([2; 2], Numbers.GetDigits(8I, 3))
        Assert.AreEqual([1; 0; 0], Numbers.GetDigits(9I, 3))
        Assert.AreEqual([1; 0; 1], Numbers.GetDigits(10I, 3))
        Assert.AreEqual([1; 0; 2], Numbers.GetDigits(11I, 3))
        Assert.AreEqual([1; 1; 0], Numbers.GetDigits(12I, 3))
        Assert.AreEqual([1; 1; 1], Numbers.GetDigits(13I, 3))
        Assert.AreEqual([1; 1; 2], Numbers.GetDigits(14I, 3))
        Assert.AreEqual([1; 2; 0], Numbers.GetDigits(15I, 3))
        Assert.AreEqual([1; 2; 1], Numbers.GetDigits(16I, 3))
        Assert.AreEqual([1; 2; 2], Numbers.GetDigits(17I, 3))
        Assert.AreEqual([2; 0; 0], Numbers.GetDigits(18I, 3))
        Assert.AreEqual([2; 0; 1], Numbers.GetDigits(19I, 3))
        Assert.AreEqual([2; 0; 2], Numbers.GetDigits(20I, 3))
        Assert.AreEqual([2; 1; 0], Numbers.GetDigits(21I, 3))
        Assert.AreEqual([2; 1; 1], Numbers.GetDigits(22I, 3))
        Assert.AreEqual([2; 1; 2], Numbers.GetDigits(23I, 3))
        Assert.AreEqual([2; 2; 0], Numbers.GetDigits(24I, 3))
        Assert.AreEqual([2; 2; 1], Numbers.GetDigits(25I, 3))
        Assert.AreEqual([2; 2; 2], Numbers.GetDigits(26I, 3))

    [<Test>]
    member public this.GetNumber() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; -1]) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; 11]) |> ignore) |> ignore
        Assert.AreEqual(0I, Numbers.GetNumber([0]))
        Assert.AreEqual(1I, Numbers.GetNumber([1]))
        Assert.AreEqual(5I, Numbers.GetNumber([5]))
        Assert.AreEqual(10I, Numbers.GetNumber([1; 0]))
        Assert.AreEqual(11I, Numbers.GetNumber([1; 1]))
        Assert.AreEqual(29I, Numbers.GetNumber([2; 9]))
        Assert.AreEqual(1234567890I, Numbers.GetNumber([1; 2; 3; 4; 5; 6; 7; 8; 9; 0]))
        Assert.AreEqual(bigint maxint, Numbers.GetNumber([2; 1; 4; 7; 4; 8; 3; 6; 4; 7]))
        Assert.AreEqual(bigint maxint64, Numbers.GetNumber([9; 2; 2; 3; 3; 7; 2; 0; 3; 6; 8; 5; 4; 7; 7; 5; 8; 0; 7]))
        Assert.AreEqual(12345678901234567890I, Numbers.GetNumber([1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 0]))

    [<Test>]
    member public this.GetNumberWithRadix() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; -1], 2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; 3], 2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; 2], -1) |> ignore) |> ignore
        Assert.Throws<NotSupportedException>(fun() -> Numbers.GetNumber([0; 1; 2], 11) |> ignore) |> ignore

    [<Test>]
    member public this.GetNumberWithRadix2() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; -1], 2) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; 2], 2) |> ignore) |> ignore
        Assert.AreEqual(0I, Numbers.GetNumber([0], 2))
        Assert.AreEqual(1I, Numbers.GetNumber([1], 2))
        Assert.AreEqual(2I, Numbers.GetNumber([1; 0], 2))
        Assert.AreEqual(3I, Numbers.GetNumber([1; 1], 2))
        Assert.AreEqual(4I, Numbers.GetNumber([1; 0; 0], 2))
        Assert.AreEqual(5I, Numbers.GetNumber([1; 0; 1], 2))
        Assert.AreEqual(6I, Numbers.GetNumber([1; 1; 0], 2))
        Assert.AreEqual(7I, Numbers.GetNumber([1; 1; 1], 2))
        Assert.AreEqual(8I, Numbers.GetNumber([1; 0; 0; 0], 2))
        Assert.AreEqual(9I, Numbers.GetNumber([1; 0; 0; 1], 2))
        Assert.AreEqual(10I, Numbers.GetNumber([1; 0; 1; 0], 2))
        Assert.AreEqual(11I, Numbers.GetNumber([1; 0; 1; 1], 2))
        Assert.AreEqual(12I, Numbers.GetNumber([1; 1; 0; 0], 2))
        Assert.AreEqual(13I, Numbers.GetNumber([1; 1; 0; 1], 2))
        Assert.AreEqual(14I, Numbers.GetNumber([1; 1; 1; 0], 2))
        Assert.AreEqual(15I, Numbers.GetNumber([1; 1; 1; 1], 2))

    [<Test>]
    member public this.GetNumberWithRadix3() =
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; -1], 3) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.GetNumber([0; 1; 3], 3) |> ignore) |> ignore
        Assert.AreEqual(0I, Numbers.GetNumber([0], 3))
        Assert.AreEqual(1I, Numbers.GetNumber([1], 3))
        Assert.AreEqual(2I, Numbers.GetNumber([2], 3))
        Assert.AreEqual(3I, Numbers.GetNumber([1; 0], 3))
        Assert.AreEqual(4I, Numbers.GetNumber([1; 1], 3))
        Assert.AreEqual(5I, Numbers.GetNumber([1; 2], 3))
        Assert.AreEqual(6I, Numbers.GetNumber([2; 0], 3))
        Assert.AreEqual(7I, Numbers.GetNumber([2; 1], 3))
        Assert.AreEqual(8I, Numbers.GetNumber([2; 2], 3))
        Assert.AreEqual(9I, Numbers.GetNumber([1; 0; 0], 3))
        Assert.AreEqual(10I, Numbers.GetNumber([1; 0; 1], 3))
        Assert.AreEqual(11I, Numbers.GetNumber([1; 0; 2], 3))
        Assert.AreEqual(12I, Numbers.GetNumber([1; 1; 0], 3))
        Assert.AreEqual(13I, Numbers.GetNumber([1; 1; 1], 3))
        Assert.AreEqual(14I, Numbers.GetNumber([1; 1; 2], 3))
        Assert.AreEqual(15I, Numbers.GetNumber([1; 2; 0], 3))
        Assert.AreEqual(16I, Numbers.GetNumber([1; 2; 1], 3))
        Assert.AreEqual(17I, Numbers.GetNumber([1; 2; 2], 3))
        Assert.AreEqual(18I, Numbers.GetNumber([2; 0; 0], 3))
        Assert.AreEqual(19I, Numbers.GetNumber([2; 0; 1], 3))
        Assert.AreEqual(20I, Numbers.GetNumber([2; 0; 2], 3))
        Assert.AreEqual(21I, Numbers.GetNumber([2; 1; 0], 3))
        Assert.AreEqual(22I, Numbers.GetNumber([2; 1; 1], 3))
        Assert.AreEqual(23I, Numbers.GetNumber([2; 1; 2], 3))
        Assert.AreEqual(24I, Numbers.GetNumber([2; 2; 0], 3))
        Assert.AreEqual(25I, Numbers.GetNumber([2; 2; 1], 3))
        Assert.AreEqual(26I, Numbers.GetNumber([2; 2; 2], 3))

    [<Test>]
    member public this.CalcFactorial() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcFactorial(-1) |> ignore) |> ignore
        Assert.AreEqual(1I, Numbers.CalcFactorial(0))
        Assert.AreEqual(1I, Numbers.CalcFactorial(1))
        Assert.AreEqual(2I, Numbers.CalcFactorial(2))
        Assert.AreEqual(6I, Numbers.CalcFactorial(3))
        Assert.AreEqual(24I, Numbers.CalcFactorial(4))
        Assert.AreEqual(120I, Numbers.CalcFactorial(5))
        Assert.AreEqual(720I, Numbers.CalcFactorial(6))
        Assert.AreEqual(5040I, Numbers.CalcFactorial(7))
        Assert.AreEqual(40320I, Numbers.CalcFactorial(8))
        Assert.AreEqual(362880I, Numbers.CalcFactorial(9))
        Assert.AreEqual(3628800I, Numbers.CalcFactorial(10))
        Assert.AreEqual(39916800I, Numbers.CalcFactorial(11))
        Assert.AreEqual(479001600I, Numbers.CalcFactorial(12))
        Assert.AreEqual(6227020800I, Numbers.CalcFactorial(13))
        Assert.AreEqual(87178291200I, Numbers.CalcFactorial(14))
        Assert.AreEqual(1307674368000I, Numbers.CalcFactorial(15))
        Assert.AreEqual(20922789888000I, Numbers.CalcFactorial(16))
        Assert.AreEqual(355687428096000I, Numbers.CalcFactorial(17))
        Assert.AreEqual(6402373705728000I, Numbers.CalcFactorial(18))
        Assert.AreEqual(121645100408832000I, Numbers.CalcFactorial(19))
        Assert.AreEqual(2432902008176640000I, Numbers.CalcFactorial(20))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcFactorial(-1L) |> ignore) |> ignore
        Assert.AreEqual(1I, Numbers.CalcFactorial(0L))
        Assert.AreEqual(1I, Numbers.CalcFactorial(1L))
        Assert.AreEqual(2I, Numbers.CalcFactorial(2L))
        Assert.AreEqual(6I, Numbers.CalcFactorial(3L))
        Assert.AreEqual(24I, Numbers.CalcFactorial(4L))
        Assert.AreEqual(120I, Numbers.CalcFactorial(5L))
        Assert.AreEqual(720I, Numbers.CalcFactorial(6L))
        Assert.AreEqual(5040I, Numbers.CalcFactorial(7L))
        Assert.AreEqual(40320I, Numbers.CalcFactorial(8L))
        Assert.AreEqual(362880I, Numbers.CalcFactorial(9L))
        Assert.AreEqual(3628800I, Numbers.CalcFactorial(10L))
        Assert.AreEqual(39916800I, Numbers.CalcFactorial(11L))
        Assert.AreEqual(479001600I, Numbers.CalcFactorial(12L))
        Assert.AreEqual(6227020800I, Numbers.CalcFactorial(13L))
        Assert.AreEqual(87178291200I, Numbers.CalcFactorial(14L))
        Assert.AreEqual(1307674368000I, Numbers.CalcFactorial(15L))
        Assert.AreEqual(20922789888000I, Numbers.CalcFactorial(16L))
        Assert.AreEqual(355687428096000I, Numbers.CalcFactorial(17L))
        Assert.AreEqual(6402373705728000I, Numbers.CalcFactorial(18L))
        Assert.AreEqual(121645100408832000I, Numbers.CalcFactorial(19L))
        Assert.AreEqual(2432902008176640000I, Numbers.CalcFactorial(20L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcFactorial(-1I) |> ignore) |> ignore
        Assert.AreEqual(1I, Numbers.CalcFactorial(0I))
        Assert.AreEqual(1I, Numbers.CalcFactorial(1I))
        Assert.AreEqual(2I, Numbers.CalcFactorial(2I))
        Assert.AreEqual(6I, Numbers.CalcFactorial(3I))
        Assert.AreEqual(24I, Numbers.CalcFactorial(4I))
        Assert.AreEqual(120I, Numbers.CalcFactorial(5I))
        Assert.AreEqual(720I, Numbers.CalcFactorial(6I))
        Assert.AreEqual(5040I, Numbers.CalcFactorial(7I))
        Assert.AreEqual(40320I, Numbers.CalcFactorial(8I))
        Assert.AreEqual(362880I, Numbers.CalcFactorial(9I))
        Assert.AreEqual(3628800I, Numbers.CalcFactorial(10I))
        Assert.AreEqual(39916800I, Numbers.CalcFactorial(11I))
        Assert.AreEqual(479001600I, Numbers.CalcFactorial(12I))
        Assert.AreEqual(6227020800I, Numbers.CalcFactorial(13I))
        Assert.AreEqual(87178291200I, Numbers.CalcFactorial(14I))
        Assert.AreEqual(1307674368000I, Numbers.CalcFactorial(15I))
        Assert.AreEqual(20922789888000I, Numbers.CalcFactorial(16I))
        Assert.AreEqual(355687428096000I, Numbers.CalcFactorial(17I))
        Assert.AreEqual(6402373705728000I, Numbers.CalcFactorial(18I))
        Assert.AreEqual(121645100408832000I, Numbers.CalcFactorial(19I))
        Assert.AreEqual(2432902008176640000I, Numbers.CalcFactorial(20I))
