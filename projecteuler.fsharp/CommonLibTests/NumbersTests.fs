namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib

[<TestFixture>]
type NumbersTests() =

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

    [<Test>]
    member public this.CalcBinomialCoeff() =
        // int
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1, -1) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1, 0) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1, 1) |> ignore) |> ignore
        Assert.AreEqual(0I, Numbers.CalcBinomialCoeff(5, -1))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(0, 0))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(1, 0))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(1, 1))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(2, 0))
        Assert.AreEqual(2I, Numbers.CalcBinomialCoeff(2, 1))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(2, 2))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(3, 0))
        Assert.AreEqual(3I, Numbers.CalcBinomialCoeff(3, 1))
        Assert.AreEqual(3I, Numbers.CalcBinomialCoeff(3, 2))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(3, 3))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(4, 0))
        Assert.AreEqual(4I, Numbers.CalcBinomialCoeff(4, 1))
        Assert.AreEqual(6I, Numbers.CalcBinomialCoeff(4, 2))
        Assert.AreEqual(4I, Numbers.CalcBinomialCoeff(4, 3))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(4, 4))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(5, 0))
        Assert.AreEqual(5I, Numbers.CalcBinomialCoeff(5, 1))
        Assert.AreEqual(10I, Numbers.CalcBinomialCoeff(5, 2))
        Assert.AreEqual(10I, Numbers.CalcBinomialCoeff(5, 3))
        Assert.AreEqual(5I, Numbers.CalcBinomialCoeff(5, 4))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(5, 5))
        Assert.AreEqual(0I, Numbers.CalcBinomialCoeff(5, 666))
        // int64
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1L, -1L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1L, 0L) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1L, 1L) |> ignore) |> ignore
        Assert.AreEqual(0I, Numbers.CalcBinomialCoeff(5L, -1L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(0L, 0L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(1L, 0L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(1L, 1L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(2L, 0L))
        Assert.AreEqual(2I, Numbers.CalcBinomialCoeff(2L, 1L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(2L, 2L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(3L, 0L))
        Assert.AreEqual(3I, Numbers.CalcBinomialCoeff(3L, 1L))
        Assert.AreEqual(3I, Numbers.CalcBinomialCoeff(3L, 2L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(3L, 3L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(4L, 0L))
        Assert.AreEqual(4I, Numbers.CalcBinomialCoeff(4L, 1L))
        Assert.AreEqual(6I, Numbers.CalcBinomialCoeff(4L, 2L))
        Assert.AreEqual(4I, Numbers.CalcBinomialCoeff(4L, 3L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(4L, 4L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(5L, 0L))
        Assert.AreEqual(5I, Numbers.CalcBinomialCoeff(5L, 1L))
        Assert.AreEqual(10I, Numbers.CalcBinomialCoeff(5L, 2L))
        Assert.AreEqual(10I, Numbers.CalcBinomialCoeff(5L, 3L))
        Assert.AreEqual(5I, Numbers.CalcBinomialCoeff(5L, 4L))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(5L, 5L))
        Assert.AreEqual(0I, Numbers.CalcBinomialCoeff(5L, 666L))
        // bigint
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1I, -1I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1I, 0I) |> ignore) |> ignore
        Assert.Throws<ArgumentOutOfRangeException>(fun() -> Numbers.CalcBinomialCoeff(-1I, 1I) |> ignore) |> ignore
        Assert.AreEqual(0I, Numbers.CalcBinomialCoeff(5I, -1I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(0I, 0I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(1I, 0I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(1I, 1I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(2I, 0I))
        Assert.AreEqual(2I, Numbers.CalcBinomialCoeff(2I, 1I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(2I, 2I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(3I, 0I))
        Assert.AreEqual(3I, Numbers.CalcBinomialCoeff(3I, 1I))
        Assert.AreEqual(3I, Numbers.CalcBinomialCoeff(3I, 2I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(3I, 3I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(4I, 0I))
        Assert.AreEqual(4I, Numbers.CalcBinomialCoeff(4I, 1I))
        Assert.AreEqual(6I, Numbers.CalcBinomialCoeff(4I, 2I))
        Assert.AreEqual(4I, Numbers.CalcBinomialCoeff(4I, 3I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(4I, 4I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(5I, 0I))
        Assert.AreEqual(5I, Numbers.CalcBinomialCoeff(5I, 1I))
        Assert.AreEqual(10I, Numbers.CalcBinomialCoeff(5I, 2I))
        Assert.AreEqual(10I, Numbers.CalcBinomialCoeff(5I, 3I))
        Assert.AreEqual(5I, Numbers.CalcBinomialCoeff(5I, 4I))
        Assert.AreEqual(1I, Numbers.CalcBinomialCoeff(5I, 5I))
        Assert.AreEqual(0I, Numbers.CalcBinomialCoeff(5I, 666I))