namespace CommonLibTests

open NUnit.Framework
open System
open CommonLib.Rational

[<TestFixture>]
type RationalNumber32Tests() =

    [<Test>]
    member public this.Create() =
        // process bad args
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber(1, 0) |> ignore) |> ignore
        // integer number
        this.Check(2, 1, new RationalNumber32(2))
        this.Check(-2, 1, new RationalNumber32(-2))
        // reduced rational number (into integer):
        this.Check(2, 1, new RationalNumber32(4, 2))
        this.Check(-2, 1, new RationalNumber32(-4, 2))
        this.Check(-2, 1, new RationalNumber32(4, -2))
        this.Check(2, 1, new RationalNumber32(-4, -2))
        // reduced rational number (into 1 / D form):
        this.Check(1, 2, new RationalNumber32(2, 4))
        this.Check(-1, 2, new RationalNumber32(-2, 4))
        this.Check(-1, 2, new RationalNumber32(2, -4))
        this.Check(1, 2, new RationalNumber32(-2, -4))
        // rational number
        this.Check(3, 7, new RationalNumber32(3, 7))
        this.Check(-3, 7, new RationalNumber32(-3, 7))
        this.Check(-3, 7, new RationalNumber32(3, -7))
        this.Check(3, 7, new RationalNumber32(-3, -7))
        this.Check(6, 9, new RationalNumber32(6, 9))
        this.Check(-6, 9, new RationalNumber32(-6, 9))
        this.Check(-6, 9, new RationalNumber32(6, -9))
        this.Check(6, 9, new RationalNumber32(-6, -9))

    [<Test>]
    member public this.IsZero() =
        Assert.IsTrue((new RationalNumber32(0, 1)).IsZero)
        Assert.IsFalse((new RationalNumber32(2, 1)).IsZero)
        Assert.IsFalse((new RationalNumber32(2, 3)).IsZero)
        Assert.IsFalse((new RationalNumber32(-2, 1)).IsZero)
        Assert.IsFalse((new RationalNumber32(-2, 3)).IsZero)

    [<Test>]
    member public this.IsInteger() =
        Assert.IsTrue((new RationalNumber32(0, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber32(1, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber32(2, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber32(-1, 1)).IsInteger)
        Assert.IsTrue((new RationalNumber32(-2, 1)).IsInteger)
        Assert.IsFalse((new RationalNumber32(1, 2)).IsInteger)
        Assert.IsFalse((new RationalNumber32(2, 3)).IsInteger)
        Assert.IsFalse((new RationalNumber32(-1, 2)).IsInteger)
        Assert.IsFalse((new RationalNumber32(-2, 3)).IsInteger)

    [<Test>]
    member public this.Reverse() =
        this.Check(1, 2, (new RationalNumber32(2)).Reverse())
        this.Check(-1, 2, (new RationalNumber32(-2)).Reverse())
        this.Check(2, 1, (new RationalNumber32(1, 2)).Reverse())
        this.Check(-2, 1, (new RationalNumber32(-1, 2)).Reverse())
        this.Check(3, 2, (new RationalNumber32(2, 3)).Reverse())
        this.Check(-3, 2, (new RationalNumber32(-2, 3)).Reverse())
        this.Check(2, 3, (new RationalNumber32(3, 2)).Reverse())
        this.Check(-2, 3, (new RationalNumber32(-3, 2)).Reverse())

    [<Test>]
    member public this.Simplify() =
        this.Check(2, 1, (new RationalNumber32(2)).Simplify())
        this.Check(-2, 1, (new RationalNumber32(-2)).Simplify())
        this.Check(2, 3, (new RationalNumber32(2, 3)).Simplify())
        this.Check(-2, 3, (new RationalNumber32(-2, 3)).Simplify())
        this.Check(2, 9, (new RationalNumber32(4, 18)).Simplify())
        this.Check(-2, 9, (new RationalNumber32(-4, 18)).Simplify())

    [<Test>]
    member public this.OpAdd() =
        // two integer numbers
        this.Check(5, 1, new RationalNumber32(2) + new RationalNumber32(3))
        this.Check(-1, 1, new RationalNumber32(2) + new RationalNumber32(-3))
        this.Check(1, 1, new RationalNumber32(-2) + new RationalNumber32(3))
        this.Check(-5, 1, new RationalNumber32(-2) + new RationalNumber32(-3))
        this.Check(5, 1, new RationalNumber32(3) + new RationalNumber32(2))
        this.Check(1, 1, new RationalNumber32(3) + new RationalNumber32(-2))
        this.Check(-1, 1, new RationalNumber32(-3) + new RationalNumber32(2))
        this.Check(-5, 1, new RationalNumber32(-3) + new RationalNumber32(-2))
        // integer & rational numbers
        this.Check(12, 5, new RationalNumber32(2) + new RationalNumber32(2, 5))
        this.Check(8, 5, new RationalNumber32(2) + new RationalNumber32(-2, 5))
        this.Check(-8, 5, new RationalNumber32(-2) + new RationalNumber32(2, 5))
        this.Check(-12, 5, new RationalNumber32(-2) + new RationalNumber32(-2, 5))
        this.Check(12, 5, new RationalNumber32(2, 5) + new RationalNumber32(2))
        this.Check(-8, 5, new RationalNumber32(2, 5) + new RationalNumber32(-2))
        this.Check(8, 5, new RationalNumber32(-2, 5) + new RationalNumber32(2))
        this.Check(-12, 5, new RationalNumber32(-2, 5) + new RationalNumber32(-2))
        // two rational numbers
        this.Check(11, 15, new RationalNumber32(1, 3) + new RationalNumber32(2, 5))
        this.Check(-1, 15, new RationalNumber32(1, 3) + new RationalNumber32(-2, 5))
        this.Check(1, 15, new RationalNumber32(-1, 3) + new RationalNumber32(2, 5))
        this.Check(-11, 15, new RationalNumber32(-1, 3) + new RationalNumber32(-2, 5))
        this.Check(11, 15, new RationalNumber32(2, 5) + new RationalNumber32(1, 3))
        this.Check(1, 15, new RationalNumber32(2, 5) + new RationalNumber32(-1, 3))
        this.Check(-1, 15, new RationalNumber32(-2, 5) + new RationalNumber32(1, 3))
        this.Check(-11, 15, new RationalNumber32(-2, 5) + new RationalNumber32(-1, 3))
        this.Check(6, 9, new RationalNumber32(1, 9) + new RationalNumber32(5, 9))
        this.Check(6, 9, new RationalNumber32(5, 9) + new RationalNumber32(1, 9))
        this.Check(12, 18, new RationalNumber32(4, 9) + new RationalNumber32(4, 18))
        this.Check(12, 18, new RationalNumber32(4, 18) + new RationalNumber32(4, 9))

    [<Test>]
    member public this.OpSub() =
        // two integer numbers
        this.Check(-1, 1, new RationalNumber32(2) - new RationalNumber32(3))
        this.Check(5, 1, new RationalNumber32(2) - new RationalNumber32(-3))
        this.Check(-5, 1, new RationalNumber32(-2) - new RationalNumber32(3))
        this.Check(1, 1, new RationalNumber32(-2) - new RationalNumber32(-3))
        this.Check(1, 1, new RationalNumber32(3) - new RationalNumber32(2))
        this.Check(5, 1, new RationalNumber32(3) - new RationalNumber32(-2))
        this.Check(-5, 1, new RationalNumber32(-3) - new RationalNumber32(2))
        this.Check(-1, 1, new RationalNumber32(-3) - new RationalNumber32(-2))
        // integer & rational numbers
        this.Check(8, 5, new RationalNumber32(2) - new RationalNumber32(2, 5))
        this.Check(12, 5, new RationalNumber32(2) - new RationalNumber32(-2, 5))
        this.Check(-12, 5, new RationalNumber32(-2) - new RationalNumber32(2, 5))
        this.Check(-8, 5, new RationalNumber32(-2) - new RationalNumber32(-2, 5))
        this.Check(-8, 5, new RationalNumber32(2, 5) - new RationalNumber32(2))
        this.Check(12, 5, new RationalNumber32(2, 5) - new RationalNumber32(-2))
        this.Check(-12, 5, new RationalNumber32(-2, 5) - new RationalNumber32(2))
        this.Check(8, 5, new RationalNumber32(-2, 5) - new RationalNumber32(-2))
        // two rational numbers
        this.Check(-1, 15, new RationalNumber32(1, 3) - new RationalNumber32(2, 5))
        this.Check(11, 15, new RationalNumber32(1, 3) - new RationalNumber32(-2, 5))
        this.Check(-11, 15, new RationalNumber32(-1, 3) - new RationalNumber32(2, 5))
        this.Check(1, 15, new RationalNumber32(-1, 3) - new RationalNumber32(-2, 5))
        this.Check(1, 15, new RationalNumber32(2, 5) - new RationalNumber32(1, 3))
        this.Check(11, 15, new RationalNumber32(2, 5) - new RationalNumber32(-1, 3))
        this.Check(-11, 15, new RationalNumber32(-2, 5) - new RationalNumber32(1, 3))
        this.Check(-1, 15, new RationalNumber32(-2, 5) - new RationalNumber32(-1, 3))
        this.Check(6, 9, new RationalNumber32(7, 9) - new RationalNumber32(1, 9))
        this.Check(-6, 9, new RationalNumber32(1, 9) - new RationalNumber32(7, 9))
        this.Check(12, 18, new RationalNumber32(14, 18) - new RationalNumber32(1, 9))
        this.Check(-12, 18, new RationalNumber32(1, 9) - new RationalNumber32(14, 18))

    [<Test>]
    member public this.OpMult() =
        // two integer numbers
        this.Check(6, 1, new RationalNumber32(2) * new RationalNumber32(3))
        this.Check(-6, 1, new RationalNumber32(2) * new RationalNumber32(-3))
        this.Check(-6, 1, new RationalNumber32(-2) * new RationalNumber32(3))
        this.Check(6, 1, new RationalNumber32(-2) * new RationalNumber32(-3))
        this.Check(6, 1, new RationalNumber32(3) * new RationalNumber32(2))
        this.Check(-6, 1, new RationalNumber32(3) * new RationalNumber32(-2))
        this.Check(-6, 1, new RationalNumber32(-3) * new RationalNumber32(2))
        this.Check(6, 1, new RationalNumber32(-3) * new RationalNumber32(-2))
        // integer & rational numbers
        this.Check(4, 5, new RationalNumber32(2) * new RationalNumber32(2, 5))
        this.Check(-4, 5, new RationalNumber32(2) * new RationalNumber32(-2, 5))
        this.Check(-4, 5, new RationalNumber32(-2) * new RationalNumber32(2, 5))
        this.Check(4, 5, new RationalNumber32(-2) * new RationalNumber32(-2, 5))
        this.Check(24, 18, new RationalNumber32(2) * new RationalNumber32(12, 18))
        this.Check(-24, 18, new RationalNumber32(2) * new RationalNumber32(-12, 18))
        this.Check(-24, 18, new RationalNumber32(-2) * new RationalNumber32(12, 18))
        this.Check(24, 18, new RationalNumber32(-2) * new RationalNumber32(-12, 18))
        // two rational numbers
        this.Check(6, 35, new RationalNumber32(3, 7) * new RationalNumber32(2, 5))
        this.Check(-6, 35, new RationalNumber32(3, 7) * new RationalNumber32(-2, 5))
        this.Check(-6, 35, new RationalNumber32(-3, 7) * new RationalNumber32(2, 5))
        this.Check(6, 35, new RationalNumber32(-3, 7) * new RationalNumber32(-2, 5))
        this.Check(6, 35, new RationalNumber32(2, 5) * new RationalNumber32(3, 7))
        this.Check(-6, 35, new RationalNumber32(2, 5) * new RationalNumber32(-3, 7))
        this.Check(-6, 35, new RationalNumber32(-2, 5) * new RationalNumber32(3, 7))
        this.Check(6, 35, new RationalNumber32(-2, 5) * new RationalNumber32(-3, 7))
        this.Check(6, 45, new RationalNumber32(2, 9) * new RationalNumber32(3, 5))
        this.Check(-6, 45, new RationalNumber32(2, 9) * new RationalNumber32(-3, 5))
        this.Check(-6, 45, new RationalNumber32(-2, 9) * new RationalNumber32(3, 5))
        this.Check(6, 45, new RationalNumber32(-2, 9) * new RationalNumber32(-3, 5))
        this.Check(6, 45, new RationalNumber32(3, 5) * new RationalNumber32(2, 9))
        this.Check(-6, 45, new RationalNumber32(3, 5) * new RationalNumber32(-2, 9))
        this.Check(-6, 45, new RationalNumber32(-3, 5) * new RationalNumber32(2, 9))
        this.Check(6, 45, new RationalNumber32(-3, 5) * new RationalNumber32(-2, 9))
        this.Check(1, 2, new RationalNumber32(2, 3) * new RationalNumber32(3, 4))
        this.Check(1, 2, new RationalNumber32(3, 4) * new RationalNumber32(2, 3))
        this.Check(2, 1, new RationalNumber32(4, 3) * new RationalNumber32(3, 2))
        this.Check(2, 1, new RationalNumber32(3, 2) * new RationalNumber32(4, 3))

    [<Test>]
    member public this.OpDiv() =
        // two integer numbers
        this.Check(2, 1, new RationalNumber32(4) / new RationalNumber32(2))
        this.Check(-2, 1, new RationalNumber32(4) / new RationalNumber32(-2))
        this.Check(-2, 1, new RationalNumber32(-4) / new RationalNumber32(2))
        this.Check(2, 1, new RationalNumber32(-4) / new RationalNumber32(-2))
        this.Check(1, 2, new RationalNumber32(2) / new RationalNumber32(4))
        this.Check(-1, 2, new RationalNumber32(2) / new RationalNumber32(-4))
        this.Check(-1, 2, new RationalNumber32(-2) / new RationalNumber32(4))
        this.Check(1, 2, new RationalNumber32(-2) / new RationalNumber32(-4))
        this.Check(3, 2, new RationalNumber32(3) / new RationalNumber32(2))
        this.Check(-3, 2, new RationalNumber32(3) / new RationalNumber32(-2))
        this.Check(-3, 2, new RationalNumber32(-3) / new RationalNumber32(2))
        this.Check(3, 2, new RationalNumber32(-3) / new RationalNumber32(-2))
        this.Check(2, 3, new RationalNumber32(2) / new RationalNumber32(3))
        this.Check(-2, 3, new RationalNumber32(2) / new RationalNumber32(-3))
        this.Check(-2, 3, new RationalNumber32(-2) / new RationalNumber32(3))
        this.Check(2, 3, new RationalNumber32(-2) / new RationalNumber32(-3))
        // integer & rational numbers
        this.Check(3, 1, new RationalNumber32(2) / new RationalNumber32(2, 3))
        this.Check(-3, 1, new RationalNumber32(2) / new RationalNumber32(-2, 3))
        this.Check(-3, 1, new RationalNumber32(-2) / new RationalNumber32(2, 3))
        this.Check(3, 1, new RationalNumber32(-2) / new RationalNumber32(-2, 3))
        this.Check(1, 3, new RationalNumber32(2, 3) / new RationalNumber32(2))
        this.Check(-1, 3, new RationalNumber32(2, 3) / new RationalNumber32(-2))
        this.Check(-1, 3, new RationalNumber32(-2, 3) / new RationalNumber32(2))
        this.Check(1, 3, new RationalNumber32(-2, 3) / new RationalNumber32(-2))
        // two rational numbers
        this.Check(6, 10, new RationalNumber32(2, 5) / new RationalNumber32(2, 3))
        this.Check(-6, 10, new RationalNumber32(2, 5) / new RationalNumber32(-2, 3))
        this.Check(-6, 10, new RationalNumber32(-2, 5) / new RationalNumber32(2, 3))
        this.Check(6, 10, new RationalNumber32(-2, 5) / new RationalNumber32(-2, 3))
        this.Check(10, 6, new RationalNumber32(2, 3) / new RationalNumber32(2, 5))
        this.Check(-10, 6, new RationalNumber32(2, 3) / new RationalNumber32(-2, 5))
        this.Check(-10, 6, new RationalNumber32(-2, 3) / new RationalNumber32(2, 5))
        this.Check(10, 6, new RationalNumber32(-2, 3) / new RationalNumber32(-2, 5))
        this.Check(2, 1, new RationalNumber32(4, 3) / new RationalNumber32(2, 3))
        this.Check(-2, 1, new RationalNumber32(4, 3) / new RationalNumber32(-2, 3))
        this.Check(-2, 1, new RationalNumber32(-4, 3) / new RationalNumber32(2, 3))
        this.Check(2, 1, new RationalNumber32(-4, 3) / new RationalNumber32(-2, 3))
        this.Check(1, 2, new RationalNumber32(2, 3) / new RationalNumber32(4, 3))
        this.Check(-1, 2, new RationalNumber32(2, 3) / new RationalNumber32(-4, 3))
        this.Check(-1, 2, new RationalNumber32(-2, 3) / new RationalNumber32(4, 3))
        this.Check(1, 2, new RationalNumber32(-2, 3) / new RationalNumber32(-4, 3))

    [<Test>]
    member public this.OpUnaryMinus() =
        // integer number
        this.Check(-4, 1, -new RationalNumber32(4))
        this.Check(4, 1, -new RationalNumber32(-4))
        this.Check(0, 1, -new RationalNumber32(0))
        // rational number
        this.Check(-2, 7, -new RationalNumber32(2, 7))
        this.Check(2, 7, -new RationalNumber32(-2, 7))

    [<Test>]
    member public this.Compare() =
        // two integer numbers
        Assert.IsTrue(new RationalNumber32(2) < new RationalNumber32(3))
        Assert.AreEqual(-1, (new RationalNumber32(2) :> IComparable).CompareTo(new RationalNumber32(3)))
        Assert.IsTrue(new RationalNumber32(-3) < new RationalNumber32(-2))
        Assert.AreEqual(-1, (new RationalNumber32(-3) :> IComparable).CompareTo(new RationalNumber32(-2)))
        Assert.IsTrue(new RationalNumber32(2) = new RationalNumber32(2))
        Assert.AreEqual(0, (new RationalNumber32(2) :> IComparable).CompareTo(new RationalNumber32(2)))
        Assert.IsTrue(new RationalNumber32(-2) = new RationalNumber32(-2))
        Assert.AreEqual(0, (new RationalNumber32(-2) :> IComparable).CompareTo(new RationalNumber32(-2)))
        Assert.IsTrue(new RationalNumber32(3) > new RationalNumber32(2))
        Assert.AreEqual(1, (new RationalNumber32(3) :> IComparable).CompareTo(new RationalNumber32(2)))
        Assert.IsTrue(new RationalNumber32(-2) > new RationalNumber32(-3))
        Assert.AreEqual(1, (new RationalNumber32(-2) :> IComparable).CompareTo(new RationalNumber32(-3)))
        // integer & rational numbers
        Assert.IsTrue(new RationalNumber32(2) < new RationalNumber32(7, 3))
        Assert.AreEqual(-1, (new RationalNumber32(2) :> IComparable).CompareTo(new RationalNumber32(7, 3)))
        Assert.IsTrue(new RationalNumber32(7, 3) < new RationalNumber32(3))
        Assert.AreEqual(-1, (new RationalNumber32(7, 3) :> IComparable).CompareTo(new RationalNumber32(3)))
        Assert.IsTrue(new RationalNumber32(-7, 3) < new RationalNumber32(-2))
        Assert.AreEqual(-1, (new RationalNumber32(-7, 3) :> IComparable).CompareTo(new RationalNumber32(-2)))
        Assert.IsTrue(new RationalNumber32(-3) < new RationalNumber32(-7, 3))
        Assert.AreEqual(-1, (new RationalNumber32(-3) :> IComparable).CompareTo(new RationalNumber32(-7, 3)))
        Assert.IsTrue(new RationalNumber32(7, 3) > new RationalNumber32(2))
        Assert.AreEqual(1, (new RationalNumber32(7, 3) :> IComparable).CompareTo(new RationalNumber32(2)))
        Assert.IsTrue(new RationalNumber32(3) > new RationalNumber32(7, 3))
        Assert.AreEqual(1, (new RationalNumber32(3) :> IComparable).CompareTo(new RationalNumber32(7, 3)))
        Assert.IsTrue(new RationalNumber32(-2) > new RationalNumber32(-7, 3))
        Assert.AreEqual(1, (new RationalNumber32(-2) :> IComparable).CompareTo(new RationalNumber32(-7, 3)))
        Assert.IsTrue(new RationalNumber32(-7, 3) > new RationalNumber32(-3))
        Assert.AreEqual(1, (new RationalNumber32(-7, 3) :> IComparable).CompareTo(new RationalNumber32(-3)))
        // two rational numbers
        Assert.IsTrue(new RationalNumber32(1, 3) < new RationalNumber32(1, 2))
        Assert.AreEqual(-1, (new RationalNumber32(1, 3) :> IComparable).CompareTo(new RationalNumber32(1, 2)))
        Assert.IsTrue(new RationalNumber32(-1, 2) < new RationalNumber32(-1, 3))
        Assert.AreEqual(-1, (new RationalNumber32(-1, 2) :> IComparable).CompareTo(new RationalNumber32(-1, 3)))
        Assert.IsTrue(new RationalNumber32(2, 5) = new RationalNumber32(2, 5))
        Assert.AreEqual(0, (new RationalNumber32(2, 5) :> IComparable).CompareTo(new RationalNumber32(2, 5)))
        Assert.IsTrue(new RationalNumber32(-2, 5) = new RationalNumber32(-2, 5))
        Assert.AreEqual(0, (new RationalNumber32(-2, 5) :> IComparable).CompareTo(new RationalNumber32(-2, 5)))
        Assert.IsTrue(new RationalNumber32(2, 5) = new RationalNumber32(4, 10))
        Assert.AreEqual(0, (new RationalNumber32(2, 5) :> IComparable).CompareTo(new RationalNumber32(4, 10)))
        Assert.IsTrue(new RationalNumber32(-2, 5) = new RationalNumber32(-4, 10))
        Assert.AreEqual(0, (new RationalNumber32(-2, 5) :> IComparable).CompareTo(new RationalNumber32(-4, 10)))
        Assert.IsTrue(new RationalNumber32(4, 10) = new RationalNumber32(2, 5))
        Assert.AreEqual(0, (new RationalNumber32(4, 10) :> IComparable).CompareTo(new RationalNumber32(2, 5)))
        Assert.IsTrue(new RationalNumber32(-4, 10) = new RationalNumber32(-2, 5))
        Assert.AreEqual(0, (new RationalNumber32(-4, 10) :> IComparable).CompareTo(new RationalNumber32(-2, 5)))
        Assert.IsTrue(new RationalNumber32(1, 2) > new RationalNumber32(1, 3))
        Assert.AreEqual(1, (new RationalNumber32(1, 2) :> IComparable).CompareTo(new RationalNumber32(1, 3)))
        Assert.IsTrue(new RationalNumber32(-1, 3) > new RationalNumber32(-1, 2))
        Assert.AreEqual(1, (new RationalNumber32(-1, 3) :> IComparable).CompareTo(new RationalNumber32(-1, 2)))


    [<Test>]
    member public this.EqualsAndGetHashCode() =
        // two integer numbers
        Assert.IsTrue((new RationalNumber32(2)).Equals(new RationalNumber32(2)))
        Assert.AreEqual((new RationalNumber32(2)).GetHashCode(), (new RationalNumber32(2)).GetHashCode())
        Assert.IsTrue((new RationalNumber32(-2)).Equals(new RationalNumber32(-2)))
        Assert.AreEqual((new RationalNumber32(-2)).GetHashCode(), (new RationalNumber32(-2)).GetHashCode())
        Assert.IsFalse((new RationalNumber32(2)).Equals(new RationalNumber32(3)))
        Assert.IsFalse((new RationalNumber32(3)).Equals(new RationalNumber32(2)))
        Assert.IsFalse((new RationalNumber32(-2)).Equals(new RationalNumber32(-3)))
        Assert.IsFalse((new RationalNumber32(-3)).Equals(new RationalNumber32(-2)))
        // integer & rational numbers
        Assert.IsFalse((new RationalNumber32(2)).Equals(new RationalNumber32(2, 3)))
        Assert.IsFalse((new RationalNumber32(2, 3)).Equals(new RationalNumber32(2)))
        Assert.IsFalse((new RationalNumber32(-2)).Equals(new RationalNumber32(-2, 3)))
        Assert.IsFalse((new RationalNumber32(-2, 3)).Equals(new RationalNumber32(-2)))
        // two rational numbers
        Assert.IsTrue((new RationalNumber32(1, 2)).Equals(new RationalNumber32(1, 2)))
        Assert.AreEqual((new RationalNumber32(1, 2)).GetHashCode(), (new RationalNumber32(1, 2)).GetHashCode())
        Assert.IsTrue((new RationalNumber32(-1, 2)).Equals(new RationalNumber32(-1, 2)))
        Assert.AreEqual((new RationalNumber32(-1, 2)).GetHashCode(), (new RationalNumber32(-1, 2)).GetHashCode())
        Assert.IsTrue((new RationalNumber32(2, 5)).Equals(new RationalNumber32(4, 10)))
        Assert.AreEqual((new RationalNumber32(2, 5)).GetHashCode(), (new RationalNumber32(4, 10)).GetHashCode())
        Assert.IsTrue((new RationalNumber32(-2, 5)).Equals(new RationalNumber32(-4, 10)))
        Assert.AreEqual((new RationalNumber32(-2, 5)).GetHashCode(), (new RationalNumber32(-4, 10)).GetHashCode())
        Assert.IsTrue((new RationalNumber32(4, 10)).Equals(new RationalNumber32(2, 5)))
        Assert.AreEqual((new RationalNumber32(4, 10)).GetHashCode(), (new RationalNumber32(2, 5)).GetHashCode())
        Assert.IsTrue((new RationalNumber32(-4, 10)).Equals(new RationalNumber32(-2, 5)))
        Assert.AreEqual((new RationalNumber32(-4, 10)).GetHashCode(), (new RationalNumber32(-2, 5)).GetHashCode())
        Assert.IsFalse((new RationalNumber32(1, 2)).Equals(new RationalNumber32(1, 3)))
        Assert.IsFalse((new RationalNumber32(-1, 2)).Equals(new RationalNumber32(-1, 3)))
        Assert.IsFalse((new RationalNumber32(1, 3)).Equals(new RationalNumber32(1, 2)))
        Assert.IsFalse((new RationalNumber32(-1, 3)).Equals(new RationalNumber32(-1, 2)))
        // rational number and other object
        Assert.IsFalse((new RationalNumber32(1, 2)).Equals(0.5))
        Assert.IsFalse((0.5).Equals(new RationalNumber32(1, 2)))
        Assert.IsFalse((new RationalNumber32(1, 2)).Equals("0.5"))
        Assert.IsFalse("0.5".Equals(new RationalNumber32(1, 2)))

    member private this.Check(extectedNumerator: int, expectedDenominator: int, actualNumber: RationalNumber32) =
        Assert.AreEqual(extectedNumerator, actualNumber.Numerator)
        Assert.AreEqual(expectedDenominator, actualNumber.Denominator)

[<TestFixture>]
type RationalNumber64Tests() =

    [<Test>]
    member public this.Create() =
        // process bad args
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber64(1L, 0L) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber64(1, 0) |> ignore) |> ignore
        // integer number
        this.Check(2L, 1L, new RationalNumber64(2L))
        this.Check(-2L, 1L, new RationalNumber64(-2L))
        this.Check(2L, 1L, new RationalNumber64(2))
        this.Check(-2L, 1L, new RationalNumber64(-2))
        // reduced rational number (into integer):
        this.Check(2L, 1L, new RationalNumber64(4L, 2L))
        this.Check(-2L, 1L, new RationalNumber64(-4L, 2L))
        this.Check(-2L, 1L, new RationalNumber64(4L, -2L))
        this.Check(2L, 1L, new RationalNumber64(-4L, -2L))
        this.Check(2L, 1L, new RationalNumber64(4, 2))
        this.Check(-2L, 1L, new RationalNumber64(-4, 2))
        this.Check(-2L, 1L, new RationalNumber64(4, -2))
        this.Check(2L, 1L, new RationalNumber64(-4, -2))
        // reduced rational number (into 1 / D form):
        this.Check(1L, 2L, new RationalNumber64(2L, 4L))
        this.Check(-1L, 2L, new RationalNumber64(-2L, 4L))
        this.Check(-1L, 2L, new RationalNumber64(2L, -4L))
        this.Check(1L, 2L, new RationalNumber64(-2L, -4L))
        this.Check(1L, 2L, new RationalNumber64(2, 4))
        this.Check(-1L, 2L, new RationalNumber64(-2, 4))
        this.Check(-1L, 2L, new RationalNumber64(2, -4))
        this.Check(1L, 2L, new RationalNumber64(-2, -4))
        // rational number
        this.Check(3L, 7L, new RationalNumber64(3L, 7L))
        this.Check(-3L, 7L, new RationalNumber64(-3L, 7L))
        this.Check(-3L, 7L, new RationalNumber64(3L, -7L))
        this.Check(3L, 7L, new RationalNumber64(-3L, -7L))
        this.Check(6L, 9L, new RationalNumber64(6L, 9L))
        this.Check(-6L, 9L, new RationalNumber64(-6L, 9L))
        this.Check(-6L, 9L, new RationalNumber64(6L, -9L))
        this.Check(6L, 9L, new RationalNumber64(-6L, -9L))
        this.Check(3L, 7L, new RationalNumber64(3, 7))
        this.Check(-3L, 7L, new RationalNumber64(-3, 7))
        this.Check(-3L, 7L, new RationalNumber64(3, -7))
        this.Check(3L, 7L, new RationalNumber64(-3, -7))
        this.Check(6L, 9L, new RationalNumber64(6, 9))
        this.Check(-6L, 9L, new RationalNumber64(-6, 9))
        this.Check(-6L, 9L, new RationalNumber64(6, -9))
        this.Check(6L, 9L, new RationalNumber64(-6, -9))

    [<Test>]
    member public this.IsZero() =
        Assert.IsTrue((new RationalNumber64(0L, 1L)).IsZero)
        Assert.IsFalse((new RationalNumber64(2L, 1L)).IsZero)
        Assert.IsFalse((new RationalNumber64(2L, 3L)).IsZero)
        Assert.IsFalse((new RationalNumber64(-2L, 1L)).IsZero)
        Assert.IsFalse((new RationalNumber64(-2L, 3L)).IsZero)

    [<Test>]
    member public this.IsInteger() =
        Assert.IsTrue((new RationalNumber64(0L, 1L)).IsInteger)
        Assert.IsTrue((new RationalNumber64(1L, 1L)).IsInteger)
        Assert.IsTrue((new RationalNumber64(2L, 1L)).IsInteger)
        Assert.IsTrue((new RationalNumber64(-1L, 1L)).IsInteger)
        Assert.IsTrue((new RationalNumber64(-2L, 1L)).IsInteger)
        Assert.IsFalse((new RationalNumber64(1L, 2L)).IsInteger)
        Assert.IsFalse((new RationalNumber64(2L, 3L)).IsInteger)
        Assert.IsFalse((new RationalNumber64(-1L, 2L)).IsInteger)
        Assert.IsFalse((new RationalNumber64(-2L, 3L)).IsInteger)

    [<Test>]
    member public this.Reverse() =
        this.Check(1L, 2L, (new RationalNumber64(2L)).Reverse())
        this.Check(-1L, 2L, (new RationalNumber64(-2L)).Reverse())
        this.Check(2L, 1L, (new RationalNumber64(1L, 2L)).Reverse())
        this.Check(-2L, 1L, (new RationalNumber64(-1L, 2L)).Reverse())
        this.Check(3L, 2L, (new RationalNumber64(2L, 3L)).Reverse())
        this.Check(-3L, 2L, (new RationalNumber64(-2L, 3L)).Reverse())
        this.Check(2L, 3L, (new RationalNumber64(3L, 2L)).Reverse())
        this.Check(-2L, 3L, (new RationalNumber64(-3L, 2L)).Reverse())

    [<Test>]
    member public this.Simplify() =
        this.Check(2L, 1L, (new RationalNumber64(2L)).Simplify())
        this.Check(-2L, 1L, (new RationalNumber64(-2L)).Simplify())
        this.Check(2L, 3L, (new RationalNumber64(2L, 3L)).Simplify())
        this.Check(-2L, 3L, (new RationalNumber64(-2L, 3L)).Simplify())
        this.Check(2L, 9L, (new RationalNumber64(4L, 18L)).Simplify())
        this.Check(-2L, 9L, (new RationalNumber64(-4L, 18L)).Simplify())

    [<Test>]
    member public this.OpAdd() =
        // two integer numbers
        this.Check(5L, 1L, new RationalNumber64(2L) + new RationalNumber64(3L))
        this.Check(-1L, 1L, new RationalNumber64(2L) + new RationalNumber64(-3L))
        this.Check(1L, 1L, new RationalNumber64(-2L) + new RationalNumber64(3L))
        this.Check(-5L, 1L, new RationalNumber64(-2L) + new RationalNumber64(-3L))
        this.Check(5L, 1L, new RationalNumber64(3L) + new RationalNumber64(2L))
        this.Check(1L, 1L, new RationalNumber64(3L) + new RationalNumber64(-2L))
        this.Check(-1L, 1L, new RationalNumber64(-3L) + new RationalNumber64(2L))
        this.Check(-5L, 1L, new RationalNumber64(-3L) + new RationalNumber64(-2L))
        // integer & rational numbers
        this.Check(12L, 5L, new RationalNumber64(2L) + new RationalNumber64(2L, 5L))
        this.Check(8L, 5L, new RationalNumber64(2L) + new RationalNumber64(-2L, 5L))
        this.Check(-8L, 5L, new RationalNumber64(-2L) + new RationalNumber64(2L, 5L))
        this.Check(-12L, 5L, new RationalNumber64(-2L) + new RationalNumber64(-2L, 5L))
        this.Check(12L, 5L, new RationalNumber64(2L, 5L) + new RationalNumber64(2L))
        this.Check(-8L, 5L, new RationalNumber64(2L, 5L) + new RationalNumber64(-2L))
        this.Check(8L, 5L, new RationalNumber64(-2L, 5L) + new RationalNumber64(2L))
        this.Check(-12L, 5L, new RationalNumber64(-2L, 5L) + new RationalNumber64(-2L))
        // two rational numbers
        this.Check(11L, 15L, new RationalNumber64(1L, 3L) + new RationalNumber64(2L, 5L))
        this.Check(-1L, 15L, new RationalNumber64(1L, 3L) + new RationalNumber64(-2L, 5L))
        this.Check(1L, 15L, new RationalNumber64(-1L, 3L) + new RationalNumber64(2L, 5L))
        this.Check(-11L, 15L, new RationalNumber64(-1L, 3L) + new RationalNumber64(-2L, 5L))
        this.Check(11L, 15L, new RationalNumber64(2L, 5L) + new RationalNumber64(1L, 3L))
        this.Check(1L, 15L, new RationalNumber64(2L, 5L) + new RationalNumber64(-1L, 3L))
        this.Check(-1L, 15L, new RationalNumber64(-2L, 5L) + new RationalNumber64(1L, 3L))
        this.Check(-11L, 15L, new RationalNumber64(-2L, 5L) + new RationalNumber64(-1L, 3L))
        this.Check(6L, 9L, new RationalNumber64(1L, 9L) + new RationalNumber64(5L, 9L))
        this.Check(6L, 9L, new RationalNumber64(5L, 9L) + new RationalNumber64(1L, 9L))
        this.Check(12L, 18L, new RationalNumber64(4L, 9L) + new RationalNumber64(4L, 18L))
        this.Check(12L, 18L, new RationalNumber64(4L, 18L) + new RationalNumber64(4L, 9L))

    [<Test>]
    member public this.OpSub() =
        // two integer numbers
        this.Check(-1L, 1L, new RationalNumber64(2L) - new RationalNumber64(3L))
        this.Check(5L, 1L, new RationalNumber64(2L) - new RationalNumber64(-3L))
        this.Check(-5L, 1L, new RationalNumber64(-2L) - new RationalNumber64(3L))
        this.Check(1L, 1L, new RationalNumber64(-2L) - new RationalNumber64(-3L))
        this.Check(1L, 1L, new RationalNumber64(3L) - new RationalNumber64(2L))
        this.Check(5L, 1L, new RationalNumber64(3L) - new RationalNumber64(-2L))
        this.Check(-5L, 1L, new RationalNumber64(-3L) - new RationalNumber64(2L))
        this.Check(-1L, 1L, new RationalNumber64(-3L) - new RationalNumber64(-2L))
        // integer & rational numbers
        this.Check(8L, 5L, new RationalNumber64(2L) - new RationalNumber64(2L, 5L))
        this.Check(12L, 5L, new RationalNumber64(2L) - new RationalNumber64(-2L, 5L))
        this.Check(-12L, 5L, new RationalNumber64(-2L) - new RationalNumber64(2L, 5L))
        this.Check(-8L, 5L, new RationalNumber64(-2L) - new RationalNumber64(-2L, 5L))
        this.Check(-8L, 5L, new RationalNumber64(2L, 5L) - new RationalNumber64(2L))
        this.Check(12L, 5L, new RationalNumber64(2L, 5L) - new RationalNumber64(-2L))
        this.Check(-12L, 5L, new RationalNumber64(-2L, 5L) - new RationalNumber64(2L))
        this.Check(8L, 5L, new RationalNumber64(-2L, 5L) - new RationalNumber64(-2L))
        // two rational numbers
        this.Check(-1L, 15L, new RationalNumber64(1L, 3L) - new RationalNumber64(2L, 5L))
        this.Check(11L, 15L, new RationalNumber64(1L, 3L) - new RationalNumber64(-2L, 5L))
        this.Check(-11L, 15L, new RationalNumber64(-1L, 3L) - new RationalNumber64(2L, 5L))
        this.Check(1L, 15L, new RationalNumber64(-1L, 3L) - new RationalNumber64(-2L, 5L))
        this.Check(1L, 15L, new RationalNumber64(2L, 5L) - new RationalNumber64(1L, 3L))
        this.Check(11L, 15L, new RationalNumber64(2L, 5L) - new RationalNumber64(-1L, 3L))
        this.Check(-11L, 15L, new RationalNumber64(-2L, 5L) - new RationalNumber64(1L, 3L))
        this.Check(-1L, 15L, new RationalNumber64(-2L, 5L) - new RationalNumber64(-1L, 3L))
        this.Check(6L, 9L, new RationalNumber64(7L, 9L) - new RationalNumber64(1L, 9L))
        this.Check(-6L, 9L, new RationalNumber64(1L, 9L) - new RationalNumber64(7L, 9L))
        this.Check(12L, 18L, new RationalNumber64(14L, 18L) - new RationalNumber64(1L, 9L))
        this.Check(-12L, 18L, new RationalNumber64(1L, 9L) - new RationalNumber64(14L, 18L))

    [<Test>]
    member public this.OpMult() =
        // two integer numbers
        this.Check(6L, 1L, new RationalNumber64(2L) * new RationalNumber64(3L))
        this.Check(-6L, 1L, new RationalNumber64(2L) * new RationalNumber64(-3L))
        this.Check(-6L, 1L, new RationalNumber64(-2L) * new RationalNumber64(3L))
        this.Check(6L, 1L, new RationalNumber64(-2L) * new RationalNumber64(-3L))
        this.Check(6L, 1L, new RationalNumber64(3L) * new RationalNumber64(2L))
        this.Check(-6L, 1L, new RationalNumber64(3L) * new RationalNumber64(-2L))
        this.Check(-6L, 1L, new RationalNumber64(-3L) * new RationalNumber64(2L))
        this.Check(6L, 1L, new RationalNumber64(-3L) * new RationalNumber64(-2L))
        // integer & rational numbers
        this.Check(4L, 5L, new RationalNumber64(2L) * new RationalNumber64(2L, 5L))
        this.Check(-4L, 5L, new RationalNumber64(2L) * new RationalNumber64(-2L, 5L))
        this.Check(-4L, 5L, new RationalNumber64(-2L) * new RationalNumber64(2L, 5L))
        this.Check(4L, 5L, new RationalNumber64(-2L) * new RationalNumber64(-2L, 5L))
        this.Check(24L, 18L, new RationalNumber64(2L) * new RationalNumber64(12L, 18L))
        this.Check(-24L, 18L, new RationalNumber64(2L) * new RationalNumber64(-12L, 18L))
        this.Check(-24L, 18L, new RationalNumber64(-2L) * new RationalNumber64(12L, 18L))
        this.Check(24L, 18L, new RationalNumber64(-2L) * new RationalNumber64(-12L, 18L))
        // two rational numbers
        this.Check(6L, 35L, new RationalNumber64(3L, 7L) * new RationalNumber64(2L, 5L))
        this.Check(-6L, 35L, new RationalNumber64(3L, 7L) * new RationalNumber64(-2L, 5L))
        this.Check(-6L, 35L, new RationalNumber64(-3L, 7L) * new RationalNumber64(2L, 5L))
        this.Check(6L, 35L, new RationalNumber64(-3L, 7L) * new RationalNumber64(-2L, 5L))
        this.Check(6L, 35L, new RationalNumber64(2L, 5L) * new RationalNumber64(3L, 7L))
        this.Check(-6L, 35L, new RationalNumber64(2L, 5L) * new RationalNumber64(-3L, 7L))
        this.Check(-6L, 35L, new RationalNumber64(-2L, 5L) * new RationalNumber64(3L, 7L))
        this.Check(6L, 35L, new RationalNumber64(-2L, 5L) * new RationalNumber64(-3L, 7L))
        this.Check(6L, 45L, new RationalNumber64(2L, 9L) * new RationalNumber64(3L, 5L))
        this.Check(-6L, 45L, new RationalNumber64(2L, 9L) * new RationalNumber64(-3L, 5L))
        this.Check(-6L, 45L, new RationalNumber64(-2L, 9L) * new RationalNumber64(3L, 5L))
        this.Check(6L, 45L, new RationalNumber64(-2L, 9L) * new RationalNumber64(-3L, 5L))
        this.Check(6L, 45L, new RationalNumber64(3L, 5L) * new RationalNumber64(2L, 9L))
        this.Check(-6L, 45L, new RationalNumber64(3L, 5L) * new RationalNumber64(-2L, 9L))
        this.Check(-6L, 45L, new RationalNumber64(-3L, 5L) * new RationalNumber64(2L, 9L))
        this.Check(6L, 45L, new RationalNumber64(-3L, 5L) * new RationalNumber64(-2L, 9L))
        this.Check(1L, 2L, new RationalNumber64(2L, 3L) * new RationalNumber64(3L, 4L))
        this.Check(1L, 2L, new RationalNumber64(3L, 4L) * new RationalNumber64(2L, 3L))
        this.Check(2L, 1L, new RationalNumber64(4L, 3L) * new RationalNumber64(3L, 2L))
        this.Check(2L, 1L, new RationalNumber64(3L, 2L) * new RationalNumber64(4L, 3L))

    [<Test>]
    member public this.OpDiv() =
        // two integer numbers
        this.Check(2L, 1L, new RationalNumber64(4L) / new RationalNumber64(2L))
        this.Check(-2L, 1L, new RationalNumber64(4L) / new RationalNumber64(-2L))
        this.Check(-2L, 1L, new RationalNumber64(-4L) / new RationalNumber64(2L))
        this.Check(2L, 1L, new RationalNumber64(-4L) / new RationalNumber64(-2L))
        this.Check(1L, 2L, new RationalNumber64(2L) / new RationalNumber64(4L))
        this.Check(-1L, 2L, new RationalNumber64(2L) / new RationalNumber64(-4L))
        this.Check(-1L, 2L, new RationalNumber64(-2L) / new RationalNumber64(4L))
        this.Check(1L, 2L, new RationalNumber64(-2L) / new RationalNumber64(-4L))
        this.Check(3L, 2L, new RationalNumber64(3L) / new RationalNumber64(2L))
        this.Check(-3L, 2L, new RationalNumber64(3L) / new RationalNumber64(-2L))
        this.Check(-3L, 2L, new RationalNumber64(-3L) / new RationalNumber64(2L))
        this.Check(3L, 2L, new RationalNumber64(-3L) / new RationalNumber64(-2L))
        this.Check(2L, 3L, new RationalNumber64(2L) / new RationalNumber64(3L))
        this.Check(-2L, 3L, new RationalNumber64(2L) / new RationalNumber64(-3L))
        this.Check(-2L, 3L, new RationalNumber64(-2L) / new RationalNumber64(3L))
        this.Check(2L, 3L, new RationalNumber64(-2L) / new RationalNumber64(-3L))
        // integer & rational numbers
        this.Check(3L, 1L, new RationalNumber64(2L) / new RationalNumber64(2L, 3L))
        this.Check(-3L, 1L, new RationalNumber64(2L) / new RationalNumber64(-2L, 3L))
        this.Check(-3L, 1L, new RationalNumber64(-2L) / new RationalNumber64(2L, 3L))
        this.Check(3L, 1L, new RationalNumber64(-2L) / new RationalNumber64(-2L, 3L))
        this.Check(1L, 3L, new RationalNumber64(2L, 3L) / new RationalNumber64(2L))
        this.Check(-1L, 3L, new RationalNumber64(2L, 3L) / new RationalNumber64(-2L))
        this.Check(-1L, 3L, new RationalNumber64(-2L, 3L) / new RationalNumber64(2L))
        this.Check(1L, 3L, new RationalNumber64(-2L, 3L) / new RationalNumber64(-2L))
        // two rational numbers
        this.Check(6L, 10L, new RationalNumber64(2L, 5L) / new RationalNumber64(2L, 3L))
        this.Check(-6L, 10L, new RationalNumber64(2L, 5L) / new RationalNumber64(-2L, 3L))
        this.Check(-6L, 10L, new RationalNumber64(-2L, 5L) / new RationalNumber64(2L, 3L))
        this.Check(6L, 10L, new RationalNumber64(-2L, 5L) / new RationalNumber64(-2L, 3L))
        this.Check(10L, 6L, new RationalNumber64(2L, 3L) / new RationalNumber64(2L, 5L))
        this.Check(-10L, 6L, new RationalNumber64(2L, 3L) / new RationalNumber64(-2L, 5L))
        this.Check(-10L, 6L, new RationalNumber64(-2L, 3L) / new RationalNumber64(2L, 5L))
        this.Check(10L, 6L, new RationalNumber64(-2L, 3L) / new RationalNumber64(-2L, 5L))
        this.Check(2L, 1L, new RationalNumber64(4L, 3L) / new RationalNumber64(2L, 3L))
        this.Check(-2L, 1L, new RationalNumber64(4L, 3L) / new RationalNumber64(-2L, 3L))
        this.Check(-2L, 1L, new RationalNumber64(-4L, 3L) / new RationalNumber64(2L, 3L))
        this.Check(2L, 1L, new RationalNumber64(-4L, 3L) / new RationalNumber64(-2L, 3L))
        this.Check(1L, 2L, new RationalNumber64(2L, 3L) / new RationalNumber64(4L, 3L))
        this.Check(-1L, 2L, new RationalNumber64(2L, 3L) / new RationalNumber64(-4L, 3L))
        this.Check(-1L, 2L, new RationalNumber64(-2L, 3L) / new RationalNumber64(4L, 3L))
        this.Check(1L, 2L, new RationalNumber64(-2L, 3L) / new RationalNumber64(-4L, 3L))

    [<Test>]
    member public this.OpUnaryMinus() =
        // integer number
        this.Check(-4L, 1L, -new RationalNumber64(4L))
        this.Check(4L, 1L, -new RationalNumber64(-4L))
        this.Check(0L, 1L, -new RationalNumber64(0L))
        // rational number
        this.Check(-2L, 7L, -new RationalNumber64(2L, 7L))
        this.Check(2L, 7L, -new RationalNumber64(-2L, 7L))

    [<Test>]
    member public this.Compare() =
        // two integer numbers
        Assert.IsTrue(new RationalNumber64(2L) < new RationalNumber64(3L))
        Assert.AreEqual(-1, (new RationalNumber64(2L) :> IComparable).CompareTo(new RationalNumber64(3L)))
        Assert.IsTrue(new RationalNumber64(-3L) < new RationalNumber64(-2L))
        Assert.AreEqual(-1, (new RationalNumber64(-3L) :> IComparable).CompareTo(new RationalNumber64(-2L)))
        Assert.IsTrue(new RationalNumber64(2L) = new RationalNumber64(2L))
        Assert.AreEqual(0, (new RationalNumber64(2L) :> IComparable).CompareTo(new RationalNumber64(2L)))
        Assert.IsTrue(new RationalNumber64(-2L) = new RationalNumber64(-2L))
        Assert.AreEqual(0, (new RationalNumber64(-2L) :> IComparable).CompareTo(new RationalNumber64(-2L)))
        Assert.IsTrue(new RationalNumber64(3L) > new RationalNumber64(2L))
        Assert.AreEqual(1, (new RationalNumber64(3L) :> IComparable).CompareTo(new RationalNumber64(2L)))
        Assert.IsTrue(new RationalNumber64(-2L) > new RationalNumber64(-3L))
        Assert.AreEqual(1, (new RationalNumber64(-2L) :> IComparable).CompareTo(new RationalNumber64(-3L)))
        // integer & rational numbers
        Assert.IsTrue(new RationalNumber64(2L) < new RationalNumber64(7L, 3L))
        Assert.AreEqual(-1, (new RationalNumber64(2L) :> IComparable).CompareTo(new RationalNumber64(7L, 3L)))
        Assert.IsTrue(new RationalNumber64(7L, 3L) < new RationalNumber64(3L))
        Assert.AreEqual(-1, (new RationalNumber64(7L, 3L) :> IComparable).CompareTo(new RationalNumber64(3L)))
        Assert.IsTrue(new RationalNumber64(-7L, 3L) < new RationalNumber64(-2L))
        Assert.AreEqual(-1, (new RationalNumber64(-7L, 3L) :> IComparable).CompareTo(new RationalNumber64(-2L)))
        Assert.IsTrue(new RationalNumber64(-3L) < new RationalNumber64(-7L, 3L))
        Assert.AreEqual(-1, (new RationalNumber64(-3L) :> IComparable).CompareTo(new RationalNumber64(-7L, 3L)))
        Assert.IsTrue(new RationalNumber64(7L, 3L) > new RationalNumber64(2L))
        Assert.AreEqual(1, (new RationalNumber64(7L, 3L) :> IComparable).CompareTo(new RationalNumber64(2L)))
        Assert.IsTrue(new RationalNumber64(3L) > new RationalNumber64(7L, 3L))
        Assert.AreEqual(1, (new RationalNumber64(3L) :> IComparable).CompareTo(new RationalNumber64(7L, 3L)))
        Assert.IsTrue(new RationalNumber64(-2L) > new RationalNumber64(-7L, 3L))
        Assert.AreEqual(1, (new RationalNumber64(-2L) :> IComparable).CompareTo(new RationalNumber64(-7L, 3L)))
        Assert.IsTrue(new RationalNumber64(-7L, 3L) > new RationalNumber64(-3L))
        Assert.AreEqual(1, (new RationalNumber64(-7L, 3L) :> IComparable).CompareTo(new RationalNumber64(-3L)))
        // two rational numbers
        Assert.IsTrue(new RationalNumber64(1L, 3L) < new RationalNumber64(1L, 2L))
        Assert.AreEqual(-1, (new RationalNumber64(1L, 3L) :> IComparable).CompareTo(new RationalNumber64(1L, 2L)))
        Assert.IsTrue(new RationalNumber64(-1L, 2L) < new RationalNumber64(-1L, 3L))
        Assert.AreEqual(-1, (new RationalNumber64(-1L, 2L) :> IComparable).CompareTo(new RationalNumber64(-1L, 3L)))
        Assert.IsTrue(new RationalNumber64(2L, 5L) = new RationalNumber64(2L, 5L))
        Assert.AreEqual(0, (new RationalNumber64(2L, 5L) :> IComparable).CompareTo(new RationalNumber64(2L, 5L)))
        Assert.IsTrue(new RationalNumber64(-2L, 5L) = new RationalNumber64(-2L, 5L))
        Assert.AreEqual(0, (new RationalNumber64(-2L, 5L) :> IComparable).CompareTo(new RationalNumber64(-2L, 5L)))
        Assert.IsTrue(new RationalNumber64(2L, 5L) = new RationalNumber64(4L, 10L))
        Assert.AreEqual(0, (new RationalNumber64(2L, 5L) :> IComparable).CompareTo(new RationalNumber64(4L, 10L)))
        Assert.IsTrue(new RationalNumber64(-2L, 5L) = new RationalNumber64(-4L, 10L))
        Assert.AreEqual(0, (new RationalNumber64(-2L, 5L) :> IComparable).CompareTo(new RationalNumber64(-4L, 10L)))
        Assert.IsTrue(new RationalNumber64(4L, 10L) = new RationalNumber64(2L, 5L))
        Assert.AreEqual(0, (new RationalNumber64(4L, 10L) :> IComparable).CompareTo(new RationalNumber64(2L, 5L)))
        Assert.IsTrue(new RationalNumber64(-4L, 10L) = new RationalNumber64(-2L, 5L))
        Assert.AreEqual(0, (new RationalNumber64(-4L, 10L) :> IComparable).CompareTo(new RationalNumber64(-2L, 5L)))
        Assert.IsTrue(new RationalNumber64(1L, 2L) > new RationalNumber64(1L, 3L))
        Assert.AreEqual(1, (new RationalNumber64(1L, 2L) :> IComparable).CompareTo(new RationalNumber64(1L, 3L)))
        Assert.IsTrue(new RationalNumber64(-1L, 3L) > new RationalNumber64(-1L, 2L))
        Assert.AreEqual(1, (new RationalNumber64(-1L, 3L) :> IComparable).CompareTo(new RationalNumber64(-1L, 2L)))


    [<Test>]
    member public this.EqualsAndGetHashCode() =
        // two integer numbers
        Assert.IsTrue((new RationalNumber64(2L)).Equals(new RationalNumber64(2L)))
        Assert.AreEqual((new RationalNumber64(2L)).GetHashCode(), (new RationalNumber64(2L)).GetHashCode())
        Assert.IsTrue((new RationalNumber64(-2L)).Equals(new RationalNumber64(-2L)))
        Assert.AreEqual((new RationalNumber64(-2L)).GetHashCode(), (new RationalNumber64(-2L)).GetHashCode())
        Assert.IsFalse((new RationalNumber64(2L)).Equals(new RationalNumber64(3L)))
        Assert.IsFalse((new RationalNumber64(3L)).Equals(new RationalNumber64(2L)))
        Assert.IsFalse((new RationalNumber64(-2L)).Equals(new RationalNumber64(-3L)))
        Assert.IsFalse((new RationalNumber64(-3L)).Equals(new RationalNumber64(-2L)))
        // integer & rational numbers
        Assert.IsFalse((new RationalNumber64(2L)).Equals(new RationalNumber64(2L, 3L)))
        Assert.IsFalse((new RationalNumber64(2L, 3L)).Equals(new RationalNumber64(2L)))
        Assert.IsFalse((new RationalNumber64(-2L)).Equals(new RationalNumber64(-2L, 3L)))
        Assert.IsFalse((new RationalNumber64(-2L, 3L)).Equals(new RationalNumber64(-2L)))
        // two rational numbers
        Assert.IsTrue((new RationalNumber64(1L, 2L)).Equals(new RationalNumber64(1L, 2L)))
        Assert.AreEqual((new RationalNumber64(1L, 2L)).GetHashCode(), (new RationalNumber64(1L, 2L)).GetHashCode())
        Assert.IsTrue((new RationalNumber64(-1L, 2L)).Equals(new RationalNumber64(-1L, 2L)))
        Assert.AreEqual((new RationalNumber64(-1L, 2L)).GetHashCode(), (new RationalNumber64(-1L, 2L)).GetHashCode())
        Assert.IsTrue((new RationalNumber64(2L, 5L)).Equals(new RationalNumber64(4L, 10L)))
        Assert.AreEqual((new RationalNumber64(2L, 5L)).GetHashCode(), (new RationalNumber64(4L, 10L)).GetHashCode())
        Assert.IsTrue((new RationalNumber64(-2L, 5L)).Equals(new RationalNumber64(-4L, 10L)))
        Assert.AreEqual((new RationalNumber64(-2L, 5L)).GetHashCode(), (new RationalNumber64(-4L, 10L)).GetHashCode())
        Assert.IsTrue((new RationalNumber64(4L, 10L)).Equals(new RationalNumber64(2L, 5L)))
        Assert.AreEqual((new RationalNumber64(4L, 10L)).GetHashCode(), (new RationalNumber64(2L, 5L)).GetHashCode())
        Assert.IsTrue((new RationalNumber64(-4L, 10L)).Equals(new RationalNumber64(-2L, 5L)))
        Assert.AreEqual((new RationalNumber64(-4L, 10L)).GetHashCode(), (new RationalNumber64(-2L, 5L)).GetHashCode())
        Assert.IsFalse((new RationalNumber64(1L, 2L)).Equals(new RationalNumber64(1L, 3L)))
        Assert.IsFalse((new RationalNumber64(-1L, 2L)).Equals(new RationalNumber64(-1L, 3L)))
        Assert.IsFalse((new RationalNumber64(1L, 3L)).Equals(new RationalNumber64(1L, 2L)))
        Assert.IsFalse((new RationalNumber64(-1L, 3L)).Equals(new RationalNumber64(-1L, 2L)))
        // rational number and other object
        Assert.IsFalse((new RationalNumber64(1L, 2L)).Equals(0.5))
        Assert.IsFalse((0.5).Equals(new RationalNumber64(1L, 2L)))
        Assert.IsFalse((new RationalNumber64(1L, 2L)).Equals("0.5"))
        Assert.IsFalse("0.5".Equals(new RationalNumber64(1L, 2L)))

    member private this.Check(extectedNumerator: int, expectedDenominator: int, actualNumber: RationalNumber64) =
        this.Check(extectedNumerator |> int64, expectedDenominator |> int64, actualNumber)

    member private this.Check(expectedNumerator: int64, expectedDenominator: int64, actualNumber: RationalNumber64) =
        Assert.AreEqual(expectedNumerator, actualNumber.Numerator)
        Assert.AreEqual(expectedDenominator, actualNumber.Denominator)

[<TestFixture>]
type RationalNumberTests() =

    [<Test>]
    member public this.Create() =
        // process bad args
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber(1I, 0I) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber(1L, 0L) |> ignore) |> ignore
        Assert.Throws<ArgumentException>(fun() -> new RationalNumber(1, 0) |> ignore) |> ignore
        // integer number
        this.Check(2I, 1I, new RationalNumber(2I))
        this.Check(-2I, 1I, new RationalNumber(-2I))
        this.Check(2I, 1I, new RationalNumber(2L))
        this.Check(-2I, 1I, new RationalNumber(-2L))
        this.Check(2I, 1I, new RationalNumber(2))
        this.Check(-2I, 1I, new RationalNumber(-2))
        // reduced rational number (into integer):
        this.Check(2I, 1I, new RationalNumber(4I, 2I))
        this.Check(-2I, 1I, new RationalNumber(-4I, 2I))
        this.Check(-2I, 1I, new RationalNumber(4I, -2I))
        this.Check(2I, 1I, new RationalNumber(-4I, -2I))
        this.Check(2I, 1I, new RationalNumber(4L, 2L))
        this.Check(-2I, 1I, new RationalNumber(-4L, 2L))
        this.Check(-2I, 1I, new RationalNumber(4L, -2L))
        this.Check(2I, 1I, new RationalNumber(-4L, -2L))
        this.Check(2I, 1I, new RationalNumber(4, 2))
        this.Check(-2I, 1I, new RationalNumber(-4, 2))
        this.Check(-2I, 1I, new RationalNumber(4, -2))
        this.Check(2I, 1I, new RationalNumber(-4, -2))
        // reduced rational number (into 1 / D form):
        this.Check(1I, 2I, new RationalNumber(2I, 4I))
        this.Check(-1I, 2I, new RationalNumber(-2I, 4I))
        this.Check(-1I, 2I, new RationalNumber(2I, -4I))
        this.Check(1I, 2I, new RationalNumber(-2I, -4I))
        this.Check(1I, 2I, new RationalNumber(2L, 4L))
        this.Check(-1I, 2I, new RationalNumber(-2L, 4L))
        this.Check(-1I, 2I, new RationalNumber(2L, -4L))
        this.Check(1I, 2I, new RationalNumber(-2L, -4L))
        this.Check(1I, 2I, new RationalNumber(2, 4))
        this.Check(-1I, 2I, new RationalNumber(-2, 4))
        this.Check(-1I, 2I, new RationalNumber(2, -4))
        this.Check(1I, 2I, new RationalNumber(-2, -4))
        // rational number
        this.Check(3I, 7I, new RationalNumber(3I, 7I))
        this.Check(-3I, 7I, new RationalNumber(-3I, 7I))
        this.Check(-3I, 7I, new RationalNumber(3I, -7I))
        this.Check(3I, 7I, new RationalNumber(-3I, -7I))
        this.Check(6I, 9I, new RationalNumber(6I, 9I))
        this.Check(-6I, 9I, new RationalNumber(-6I, 9I))
        this.Check(-6I, 9I, new RationalNumber(6I, -9I))
        this.Check(6I, 9I, new RationalNumber(-6I, -9I))
        this.Check(3I, 7I, new RationalNumber(3L, 7L))
        this.Check(-3I, 7I, new RationalNumber(-3L, 7L))
        this.Check(-3I, 7I, new RationalNumber(3L, -7L))
        this.Check(3I, 7I, new RationalNumber(-3L, -7L))
        this.Check(6I, 9I, new RationalNumber(6L, 9L))
        this.Check(-6I, 9I, new RationalNumber(-6L, 9L))
        this.Check(-6I, 9I, new RationalNumber(6L, -9L))
        this.Check(6I, 9I, new RationalNumber(-6L, -9L))
        this.Check(3I, 7I, new RationalNumber(3, 7))
        this.Check(-3I, 7I, new RationalNumber(-3, 7))
        this.Check(-3I, 7I, new RationalNumber(3, -7))
        this.Check(3I, 7I, new RationalNumber(-3, -7))
        this.Check(6I, 9I, new RationalNumber(6, 9))
        this.Check(-6I, 9I, new RationalNumber(-6, 9))
        this.Check(-6I, 9I, new RationalNumber(6, -9))
        this.Check(6I, 9I, new RationalNumber(-6, -9))

    [<Test>]
    member public this.IsZero() =
        Assert.IsTrue((new RationalNumber(0I, 1I)).IsZero)
        Assert.IsFalse((new RationalNumber(2I, 1I)).IsZero)
        Assert.IsFalse((new RationalNumber(2I, 3I)).IsZero)
        Assert.IsFalse((new RationalNumber(-2I, 1I)).IsZero)
        Assert.IsFalse((new RationalNumber(-2I, 3I)).IsZero)

    [<Test>]
    member public this.IsInteger() =
        Assert.IsTrue((new RationalNumber(0I, 1I)).IsInteger)
        Assert.IsTrue((new RationalNumber(1I, 1I)).IsInteger)
        Assert.IsTrue((new RationalNumber(2I, 1I)).IsInteger)
        Assert.IsTrue((new RationalNumber(-1I, 1I)).IsInteger)
        Assert.IsTrue((new RationalNumber(-2I, 1I)).IsInteger)
        Assert.IsFalse((new RationalNumber(1I, 2I)).IsInteger)
        Assert.IsFalse((new RationalNumber(2I, 3I)).IsInteger)
        Assert.IsFalse((new RationalNumber(-1I, 2I)).IsInteger)
        Assert.IsFalse((new RationalNumber(-2I, 3I)).IsInteger)

    [<Test>]
    member public this.Reverse() =
        this.Check(1I, 2I, (new RationalNumber(2I)).Reverse())
        this.Check(-1I, 2I, (new RationalNumber(-2I)).Reverse())
        this.Check(2I, 1I, (new RationalNumber(1I, 2I)).Reverse())
        this.Check(-2I, 1I, (new RationalNumber(-1I, 2I)).Reverse())
        this.Check(3I, 2I, (new RationalNumber(2I, 3I)).Reverse())
        this.Check(-3I, 2I, (new RationalNumber(-2I, 3I)).Reverse())
        this.Check(2I, 3I, (new RationalNumber(3I, 2I)).Reverse())
        this.Check(-2I, 3I, (new RationalNumber(-3I, 2I)).Reverse())

    [<Test>]
    member public this.Simplify() =
        this.Check(2I, 1I, (new RationalNumber(2I)).Simplify())
        this.Check(-2I, 1I, (new RationalNumber(-2I)).Simplify())
        this.Check(2I, 3I, (new RationalNumber(2I, 3I)).Simplify())
        this.Check(-2I, 3I, (new RationalNumber(-2I, 3I)).Simplify())
        this.Check(2I, 9I, (new RationalNumber(4I, 18I)).Simplify())
        this.Check(-2I, 9I, (new RationalNumber(-4I, 18I)).Simplify())

    [<Test>]
    member public this.OpAdd() =
        // two integer numbers
        this.Check(5I, 1I, new RationalNumber(2I) + new RationalNumber(3I))
        this.Check(-1I, 1I, new RationalNumber(2I) + new RationalNumber(-3I))
        this.Check(1I, 1I, new RationalNumber(-2I) + new RationalNumber(3I))
        this.Check(-5I, 1I, new RationalNumber(-2I) + new RationalNumber(-3I))
        this.Check(5I, 1I, new RationalNumber(3I) + new RationalNumber(2I))
        this.Check(1I, 1I, new RationalNumber(3I) + new RationalNumber(-2I))
        this.Check(-1I, 1I, new RationalNumber(-3I) + new RationalNumber(2I))
        this.Check(-5I, 1I, new RationalNumber(-3I) + new RationalNumber(-2I))
        // integer & rational numbers
        this.Check(12I, 5I, new RationalNumber(2I) + new RationalNumber(2I, 5I))
        this.Check(8I, 5I, new RationalNumber(2I) + new RationalNumber(-2I, 5I))
        this.Check(-8I, 5I, new RationalNumber(-2I) + new RationalNumber(2I, 5I))
        this.Check(-12I, 5I, new RationalNumber(-2I) + new RationalNumber(-2I, 5I))
        this.Check(12I, 5I, new RationalNumber(2I, 5I) + new RationalNumber(2I))
        this.Check(-8I, 5I, new RationalNumber(2I, 5I) + new RationalNumber(-2I))
        this.Check(8I, 5I, new RationalNumber(-2I, 5I) + new RationalNumber(2I))
        this.Check(-12I, 5I, new RationalNumber(-2I, 5I) + new RationalNumber(-2I))
        // two rational numbers
        this.Check(11I, 15I, new RationalNumber(1I, 3I) + new RationalNumber(2I, 5I))
        this.Check(-1I, 15I, new RationalNumber(1I, 3I) + new RationalNumber(-2I, 5I))
        this.Check(1I, 15I, new RationalNumber(-1I, 3I) + new RationalNumber(2I, 5I))
        this.Check(-11I, 15I, new RationalNumber(-1I, 3I) + new RationalNumber(-2I, 5I))
        this.Check(11I, 15I, new RationalNumber(2I, 5I) + new RationalNumber(1I, 3I))
        this.Check(1I, 15I, new RationalNumber(2I, 5I) + new RationalNumber(-1I, 3I))
        this.Check(-1I, 15I, new RationalNumber(-2I, 5I) + new RationalNumber(1I, 3I))
        this.Check(-11I, 15I, new RationalNumber(-2I, 5I) + new RationalNumber(-1I, 3I))
        this.Check(6I, 9I, new RationalNumber(1I, 9I) + new RationalNumber(5I, 9I))
        this.Check(6I, 9I, new RationalNumber(5I, 9I) + new RationalNumber(1I, 9I))
        this.Check(12I, 18I, new RationalNumber(4I, 9I) + new RationalNumber(4I, 18I))
        this.Check(12I, 18I, new RationalNumber(4I, 18I) + new RationalNumber(4I, 9I))

    [<Test>]
    member public this.OpSub() =
        // two integer numbers
        this.Check(-1I, 1I, new RationalNumber(2I) - new RationalNumber(3I))
        this.Check(5I, 1I, new RationalNumber(2I) - new RationalNumber(-3I))
        this.Check(-5I, 1I, new RationalNumber(-2I) - new RationalNumber(3I))
        this.Check(1I, 1I, new RationalNumber(-2I) - new RationalNumber(-3I))
        this.Check(1I, 1I, new RationalNumber(3I) - new RationalNumber(2I))
        this.Check(5I, 1I, new RationalNumber(3I) - new RationalNumber(-2I))
        this.Check(-5I, 1I, new RationalNumber(-3I) - new RationalNumber(2I))
        this.Check(-1I, 1I, new RationalNumber(-3I) - new RationalNumber(-2I))
        // integer & rational numbers
        this.Check(8I, 5I, new RationalNumber(2I) - new RationalNumber(2I, 5I))
        this.Check(12I, 5I, new RationalNumber(2I) - new RationalNumber(-2I, 5I))
        this.Check(-12I, 5I, new RationalNumber(-2I) - new RationalNumber(2I, 5I))
        this.Check(-8I, 5I, new RationalNumber(-2I) - new RationalNumber(-2I, 5I))
        this.Check(-8I, 5I, new RationalNumber(2I, 5I) - new RationalNumber(2I))
        this.Check(12I, 5I, new RationalNumber(2I, 5I) - new RationalNumber(-2I))
        this.Check(-12I, 5I, new RationalNumber(-2I, 5I) - new RationalNumber(2I))
        this.Check(8I, 5I, new RationalNumber(-2I, 5I) - new RationalNumber(-2I))
        // two rational numbers
        this.Check(-1I, 15I, new RationalNumber(1I, 3I) - new RationalNumber(2I, 5I))
        this.Check(11I, 15I, new RationalNumber(1I, 3I) - new RationalNumber(-2I, 5I))
        this.Check(-11I, 15I, new RationalNumber(-1I, 3I) - new RationalNumber(2I, 5I))
        this.Check(1I, 15I, new RationalNumber(-1I, 3I) - new RationalNumber(-2I, 5I))
        this.Check(1I, 15I, new RationalNumber(2I, 5I) - new RationalNumber(1I, 3I))
        this.Check(11I, 15I, new RationalNumber(2I, 5I) - new RationalNumber(-1I, 3I))
        this.Check(-11I, 15I, new RationalNumber(-2I, 5I) - new RationalNumber(1I, 3I))
        this.Check(-1I, 15I, new RationalNumber(-2I, 5I) - new RationalNumber(-1I, 3I))
        this.Check(6I, 9I, new RationalNumber(7I, 9I) - new RationalNumber(1I, 9I))
        this.Check(-6I, 9I, new RationalNumber(1I, 9I) - new RationalNumber(7I, 9I))
        this.Check(12I, 18I, new RationalNumber(14I, 18I) - new RationalNumber(1I, 9I))
        this.Check(-12I, 18I, new RationalNumber(1I, 9I) - new RationalNumber(14I, 18I))

    [<Test>]
    member public this.OpMult() =
        // two integer numbers
        this.Check(6I, 1I, new RationalNumber(2I) * new RationalNumber(3I))
        this.Check(-6I, 1I, new RationalNumber(2I) * new RationalNumber(-3I))
        this.Check(-6I, 1I, new RationalNumber(-2I) * new RationalNumber(3I))
        this.Check(6I, 1I, new RationalNumber(-2I) * new RationalNumber(-3I))
        this.Check(6I, 1I, new RationalNumber(3I) * new RationalNumber(2I))
        this.Check(-6I, 1I, new RationalNumber(3I) * new RationalNumber(-2I))
        this.Check(-6I, 1I, new RationalNumber(-3I) * new RationalNumber(2I))
        this.Check(6I, 1I, new RationalNumber(-3I) * new RationalNumber(-2I))
        // integer & rational numbers
        this.Check(4I, 5I, new RationalNumber(2I) * new RationalNumber(2I, 5I))
        this.Check(-4I, 5I, new RationalNumber(2I) * new RationalNumber(-2I, 5I))
        this.Check(-4I, 5I, new RationalNumber(-2I) * new RationalNumber(2I, 5I))
        this.Check(4I, 5I, new RationalNumber(-2I) * new RationalNumber(-2I, 5I))
        this.Check(24I, 18I, new RationalNumber(2I) * new RationalNumber(12I, 18I))
        this.Check(-24I, 18I, new RationalNumber(2I) * new RationalNumber(-12I, 18I))
        this.Check(-24I, 18I, new RationalNumber(-2I) * new RationalNumber(12I, 18I))
        this.Check(24I, 18I, new RationalNumber(-2I) * new RationalNumber(-12I, 18I))
        // two rational numbers
        this.Check(6I, 35I, new RationalNumber(3I, 7I) * new RationalNumber(2I, 5I))
        this.Check(-6I, 35I, new RationalNumber(3I, 7I) * new RationalNumber(-2I, 5I))
        this.Check(-6I, 35I, new RationalNumber(-3I, 7I) * new RationalNumber(2I, 5I))
        this.Check(6I, 35I, new RationalNumber(-3I, 7I) * new RationalNumber(-2I, 5I))
        this.Check(6I, 35I, new RationalNumber(2I, 5I) * new RationalNumber(3I, 7I))
        this.Check(-6I, 35I, new RationalNumber(2I, 5I) * new RationalNumber(-3I, 7I))
        this.Check(-6I, 35I, new RationalNumber(-2I, 5I) * new RationalNumber(3I, 7I))
        this.Check(6I, 35I, new RationalNumber(-2I, 5I) * new RationalNumber(-3I, 7I))
        this.Check(6I, 45I, new RationalNumber(2I, 9I) * new RationalNumber(3I, 5I))
        this.Check(-6I, 45I, new RationalNumber(2I, 9I) * new RationalNumber(-3I, 5I))
        this.Check(-6I, 45I, new RationalNumber(-2I, 9I) * new RationalNumber(3I, 5I))
        this.Check(6I, 45I, new RationalNumber(-2I, 9I) * new RationalNumber(-3I, 5I))
        this.Check(6I, 45I, new RationalNumber(3I, 5I) * new RationalNumber(2I, 9I))
        this.Check(-6I, 45I, new RationalNumber(3I, 5I) * new RationalNumber(-2I, 9I))
        this.Check(-6I, 45I, new RationalNumber(-3I, 5I) * new RationalNumber(2I, 9I))
        this.Check(6I, 45I, new RationalNumber(-3I, 5I) * new RationalNumber(-2I, 9I))
        this.Check(1I, 2I, new RationalNumber(2I, 3I) * new RationalNumber(3I, 4I))
        this.Check(1I, 2I, new RationalNumber(3I, 4I) * new RationalNumber(2I, 3I))
        this.Check(2I, 1I, new RationalNumber(4I, 3I) * new RationalNumber(3I, 2I))
        this.Check(2I, 1I, new RationalNumber(3I, 2I) * new RationalNumber(4I, 3I))

    [<Test>]
    member public this.OpDiv() =
        // two integer numbers
        this.Check(2I, 1I, new RationalNumber(4I) / new RationalNumber(2I))
        this.Check(-2I, 1I, new RationalNumber(4I) / new RationalNumber(-2I))
        this.Check(-2I, 1I, new RationalNumber(-4I) / new RationalNumber(2I))
        this.Check(2I, 1I, new RationalNumber(-4I) / new RationalNumber(-2I))
        this.Check(1I, 2I, new RationalNumber(2I) / new RationalNumber(4I))
        this.Check(-1I, 2I, new RationalNumber(2I) / new RationalNumber(-4I))
        this.Check(-1I, 2I, new RationalNumber(-2I) / new RationalNumber(4I))
        this.Check(1I, 2I, new RationalNumber(-2I) / new RationalNumber(-4I))
        this.Check(3I, 2I, new RationalNumber(3I) / new RationalNumber(2I))
        this.Check(-3I, 2I, new RationalNumber(3I) / new RationalNumber(-2I))
        this.Check(-3I, 2I, new RationalNumber(-3I) / new RationalNumber(2I))
        this.Check(3I, 2I, new RationalNumber(-3I) / new RationalNumber(-2I))
        this.Check(2I, 3I, new RationalNumber(2I) / new RationalNumber(3I))
        this.Check(-2I, 3I, new RationalNumber(2I) / new RationalNumber(-3I))
        this.Check(-2I, 3I, new RationalNumber(-2I) / new RationalNumber(3I))
        this.Check(2I, 3I, new RationalNumber(-2I) / new RationalNumber(-3I))
        // integer & rational numbers
        this.Check(3I, 1I, new RationalNumber(2I) / new RationalNumber(2I, 3I))
        this.Check(-3I, 1I, new RationalNumber(2I) / new RationalNumber(-2I, 3I))
        this.Check(-3I, 1I, new RationalNumber(-2I) / new RationalNumber(2I, 3I))
        this.Check(3I, 1I, new RationalNumber(-2I) / new RationalNumber(-2I, 3I))
        this.Check(1I, 3I, new RationalNumber(2I, 3I) / new RationalNumber(2I))
        this.Check(-1I, 3I, new RationalNumber(2I, 3I) / new RationalNumber(-2I))
        this.Check(-1I, 3I, new RationalNumber(-2I, 3I) / new RationalNumber(2I))
        this.Check(1I, 3I, new RationalNumber(-2I, 3I) / new RationalNumber(-2I))
        // two rational numbers
        this.Check(6I, 10I, new RationalNumber(2I, 5I) / new RationalNumber(2I, 3I))
        this.Check(-6I, 10I, new RationalNumber(2I, 5I) / new RationalNumber(-2I, 3I))
        this.Check(-6I, 10I, new RationalNumber(-2I, 5I) / new RationalNumber(2I, 3I))
        this.Check(6I, 10I, new RationalNumber(-2I, 5I) / new RationalNumber(-2I, 3I))
        this.Check(10I, 6I, new RationalNumber(2I, 3I) / new RationalNumber(2I, 5I))
        this.Check(-10I, 6I, new RationalNumber(2I, 3I) / new RationalNumber(-2I, 5I))
        this.Check(-10I, 6I, new RationalNumber(-2I, 3I) / new RationalNumber(2I, 5I))
        this.Check(10I, 6I, new RationalNumber(-2I, 3I) / new RationalNumber(-2I, 5I))
        this.Check(2I, 1I, new RationalNumber(4I, 3I) / new RationalNumber(2I, 3I))
        this.Check(-2I, 1I, new RationalNumber(4I, 3I) / new RationalNumber(-2I, 3I))
        this.Check(-2I, 1I, new RationalNumber(-4I, 3I) / new RationalNumber(2I, 3I))
        this.Check(2I, 1I, new RationalNumber(-4I, 3I) / new RationalNumber(-2I, 3I))
        this.Check(1I, 2I, new RationalNumber(2I, 3I) / new RationalNumber(4I, 3I))
        this.Check(-1I, 2I, new RationalNumber(2I, 3I) / new RationalNumber(-4I, 3I))
        this.Check(-1I, 2I, new RationalNumber(-2I, 3I) / new RationalNumber(4I, 3I))
        this.Check(1I, 2I, new RationalNumber(-2I, 3I) / new RationalNumber(-4I, 3I))

    [<Test>]
    member public this.OpUnaryMinus() =
        // integer number
        this.Check(-4I, 1I, -new RationalNumber(4I))
        this.Check(4I, 1I, -new RationalNumber(-4I))
        this.Check(0I, 1I, -new RationalNumber(0I))
        // rational number
        this.Check(-2I, 7I, -new RationalNumber(2I, 7I))
        this.Check(2I, 7I, -new RationalNumber(-2I, 7I))

    [<Test>]
    member public this.Compare() =
        // two integer numbers
        Assert.IsTrue(new RationalNumber(2I) < new RationalNumber(3I))
        Assert.AreEqual(-1, (new RationalNumber(2I) :> IComparable).CompareTo(new RationalNumber(3I)))
        Assert.IsTrue(new RationalNumber(-3I) < new RationalNumber(-2I))
        Assert.AreEqual(-1, (new RationalNumber(-3I) :> IComparable).CompareTo(new RationalNumber(-2I)))
        Assert.IsTrue(new RationalNumber(2I) = new RationalNumber(2I))
        Assert.AreEqual(0, (new RationalNumber(2I) :> IComparable).CompareTo(new RationalNumber(2I)))
        Assert.IsTrue(new RationalNumber(-2I) = new RationalNumber(-2I))
        Assert.AreEqual(0, (new RationalNumber(-2I) :> IComparable).CompareTo(new RationalNumber(-2I)))
        Assert.IsTrue(new RationalNumber(3I) > new RationalNumber(2I))
        Assert.AreEqual(1, (new RationalNumber(3I) :> IComparable).CompareTo(new RationalNumber(2I)))
        Assert.IsTrue(new RationalNumber(-2I) > new RationalNumber(-3I))
        Assert.AreEqual(1, (new RationalNumber(-2I) :> IComparable).CompareTo(new RationalNumber(-3I)))
        // integer & rational numbers
        Assert.IsTrue(new RationalNumber(2I) < new RationalNumber(7I, 3I))
        Assert.AreEqual(-1, (new RationalNumber(2I) :> IComparable).CompareTo(new RationalNumber(7I, 3I)))
        Assert.IsTrue(new RationalNumber(7I, 3I) < new RationalNumber(3I))
        Assert.AreEqual(-1, (new RationalNumber(7I, 3I) :> IComparable).CompareTo(new RationalNumber(3I)))
        Assert.IsTrue(new RationalNumber(-7I, 3I) < new RationalNumber(-2I))
        Assert.AreEqual(-1, (new RationalNumber(-7I, 3I) :> IComparable).CompareTo(new RationalNumber(-2I)))
        Assert.IsTrue(new RationalNumber(-3I) < new RationalNumber(-7I, 3I))
        Assert.AreEqual(-1, (new RationalNumber(-3I) :> IComparable).CompareTo(new RationalNumber(-7I, 3I)))
        Assert.IsTrue(new RationalNumber(7I, 3I) > new RationalNumber(2I))
        Assert.AreEqual(1, (new RationalNumber(7I, 3I) :> IComparable).CompareTo(new RationalNumber(2I)))
        Assert.IsTrue(new RationalNumber(3I) > new RationalNumber(7I, 3I))
        Assert.AreEqual(1, (new RationalNumber(3I) :> IComparable).CompareTo(new RationalNumber(7I, 3I)))
        Assert.IsTrue(new RationalNumber(-2I) > new RationalNumber(-7I, 3I))
        Assert.AreEqual(1, (new RationalNumber(-2I) :> IComparable).CompareTo(new RationalNumber(-7I, 3I)))
        Assert.IsTrue(new RationalNumber(-7I, 3I) > new RationalNumber(-3I))
        Assert.AreEqual(1, (new RationalNumber(-7I, 3I) :> IComparable).CompareTo(new RationalNumber(-3I)))
        // two rational numbers
        Assert.IsTrue(new RationalNumber(1I, 3I) < new RationalNumber(1I, 2I))
        Assert.AreEqual(-1, (new RationalNumber(1I, 3I) :> IComparable).CompareTo(new RationalNumber(1I, 2I)))
        Assert.IsTrue(new RationalNumber(-1I, 2I) < new RationalNumber(-1I, 3I))
        Assert.AreEqual(-1, (new RationalNumber(-1I, 2I) :> IComparable).CompareTo(new RationalNumber(-1I, 3I)))
        Assert.IsTrue(new RationalNumber(2I, 5I) = new RationalNumber(2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(2I, 5I) :> IComparable).CompareTo(new RationalNumber(2I, 5I)))
        Assert.IsTrue(new RationalNumber(-2I, 5I) = new RationalNumber(-2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(-2I, 5I) :> IComparable).CompareTo(new RationalNumber(-2I, 5I)))
        Assert.IsTrue(new RationalNumber(2I, 5I) = new RationalNumber(4I, 10I))
        Assert.AreEqual(0, (new RationalNumber(2I, 5I) :> IComparable).CompareTo(new RationalNumber(4I, 10I)))
        Assert.IsTrue(new RationalNumber(-2I, 5I) = new RationalNumber(-4I, 10I))
        Assert.AreEqual(0, (new RationalNumber(-2I, 5I) :> IComparable).CompareTo(new RationalNumber(-4I, 10I)))
        Assert.IsTrue(new RationalNumber(4I, 10I) = new RationalNumber(2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(4I, 10I) :> IComparable).CompareTo(new RationalNumber(2I, 5I)))
        Assert.IsTrue(new RationalNumber(-4I, 10I) = new RationalNumber(-2I, 5I))
        Assert.AreEqual(0, (new RationalNumber(-4I, 10I) :> IComparable).CompareTo(new RationalNumber(-2I, 5I)))
        Assert.IsTrue(new RationalNumber(1I, 2I) > new RationalNumber(1I, 3I))
        Assert.AreEqual(1, (new RationalNumber(1I, 2I) :> IComparable).CompareTo(new RationalNumber(1I, 3I)))
        Assert.IsTrue(new RationalNumber(-1I, 3I) > new RationalNumber(-1I, 2I))
        Assert.AreEqual(1, (new RationalNumber(-1I, 3I) :> IComparable).CompareTo(new RationalNumber(-1I, 2I)))


    [<Test>]
    member public this.EqualsAndGetHashCode() =
        // two integer numbers
        Assert.IsTrue((new RationalNumber(2I)).Equals(new RationalNumber(2I)))
        Assert.AreEqual((new RationalNumber(2I)).GetHashCode(), (new RationalNumber(2I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-2I)).Equals(new RationalNumber(-2I)))
        Assert.AreEqual((new RationalNumber(-2I)).GetHashCode(), (new RationalNumber(-2I)).GetHashCode())
        Assert.IsFalse((new RationalNumber(2I)).Equals(new RationalNumber(3I)))
        Assert.IsFalse((new RationalNumber(3I)).Equals(new RationalNumber(2I)))
        Assert.IsFalse((new RationalNumber(-2I)).Equals(new RationalNumber(-3I)))
        Assert.IsFalse((new RationalNumber(-3I)).Equals(new RationalNumber(-2I)))
        // integer & rational numbers
        Assert.IsFalse((new RationalNumber(2I)).Equals(new RationalNumber(2I, 3I)))
        Assert.IsFalse((new RationalNumber(2I, 3I)).Equals(new RationalNumber(2I)))
        Assert.IsFalse((new RationalNumber(-2I)).Equals(new RationalNumber(-2I, 3I)))
        Assert.IsFalse((new RationalNumber(-2I, 3I)).Equals(new RationalNumber(-2I)))
        // two rational numbers
        Assert.IsTrue((new RationalNumber(1I, 2I)).Equals(new RationalNumber(1I, 2I)))
        Assert.AreEqual((new RationalNumber(1I, 2I)).GetHashCode(), (new RationalNumber(1I, 2I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-1I, 2I)).Equals(new RationalNumber(-1I, 2I)))
        Assert.AreEqual((new RationalNumber(-1I, 2I)).GetHashCode(), (new RationalNumber(-1I, 2I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(2I, 5I)).Equals(new RationalNumber(4I, 10I)))
        Assert.AreEqual((new RationalNumber(2I, 5I)).GetHashCode(), (new RationalNumber(4I, 10I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-2I, 5I)).Equals(new RationalNumber(-4I, 10I)))
        Assert.AreEqual((new RationalNumber(-2I, 5I)).GetHashCode(), (new RationalNumber(-4I, 10I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(4I, 10I)).Equals(new RationalNumber(2I, 5I)))
        Assert.AreEqual((new RationalNumber(4I, 10I)).GetHashCode(), (new RationalNumber(2I, 5I)).GetHashCode())
        Assert.IsTrue((new RationalNumber(-4I, 10I)).Equals(new RationalNumber(-2I, 5I)))
        Assert.AreEqual((new RationalNumber(-4I, 10I)).GetHashCode(), (new RationalNumber(-2I, 5I)).GetHashCode())
        Assert.IsFalse((new RationalNumber(1I, 2I)).Equals(new RationalNumber(1I, 3I)))
        Assert.IsFalse((new RationalNumber(-1I, 2I)).Equals(new RationalNumber(-1I, 3I)))
        Assert.IsFalse((new RationalNumber(1I, 3I)).Equals(new RationalNumber(1I, 2I)))
        Assert.IsFalse((new RationalNumber(-1I, 3I)).Equals(new RationalNumber(-1I, 2I)))
        // rational number and other object
        Assert.IsFalse((new RationalNumber(1I, 2I)).Equals(0.5))
        Assert.IsFalse((0.5).Equals(new RationalNumber(1I, 2I)))
        Assert.IsFalse((new RationalNumber(1I, 2I)).Equals("0.5"))
        Assert.IsFalse("0.5".Equals(new RationalNumber(1I, 2I)))

    member private this.Check(extectedNumerator: int, expectedDenominator: int, actualNumber: RationalNumber) =
        this.Check(extectedNumerator |> bigint, expectedDenominator |> bigint, actualNumber)

    member private this.Check(extectedNumerator: int64, expectedDenominator: int64, actualNumber: RationalNumber) =
        this.Check(extectedNumerator |> bigint, expectedDenominator |> bigint, actualNumber)

    member private this.Check(expectedNumerator: bigint, expectedDenominator: bigint, actualNumber: RationalNumber) =
        Assert.AreEqual(expectedNumerator, actualNumber.Numerator)
        Assert.AreEqual(expectedDenominator, actualNumber.Denominator)